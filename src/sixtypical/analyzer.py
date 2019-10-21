# encoding: UTF-8

from sixtypical.ast import (
    Program, Routine, Block, SingleOp, Reset, Call, GoTo, If, Repeat, For, WithInterruptsOff, Save, PointInto
)
from sixtypical.context import AnalysisContext
from sixtypical.model import (
    TYPE_BYTE, TYPE_WORD,
    TableType, PointerType, VectorType, RoutineType,
    ConstantRef, LocationRef, IndirectRef, IndexedRef,
    REG_A, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)


class StaticAnalysisError(ValueError):
    def __init__(self, ast, message):
        super(StaticAnalysisError, self).__init__(ast, message)

    def __str__(self):
        ast = self.args[0]
        message = self.args[1]
        if isinstance(ast, Routine):
            return "{} (in {}, line {})".format(message, ast.name, ast.line_number)
        else:
            return "{} (line {})".format(message, ast.line_number)


class UnmeaningfulReadError(StaticAnalysisError):
    pass


class UnmeaningfulOutputError(StaticAnalysisError):
    pass


class InconsistentInitializationError(StaticAnalysisError):
    pass


class InconsistentExitError(StaticAnalysisError):
    """The type context differs at two different exit points of the routine."""
    pass


class ForbiddenWriteError(StaticAnalysisError):
    pass


class TypeMismatchError(StaticAnalysisError):
    pass


class IllegalJumpError(StaticAnalysisError):
    pass


class TerminatedContextError(StaticAnalysisError):
    """What the program is doing here is not valid, due to preceding `goto`s,
    which make this dead code."""
    pass


class RangeExceededError(StaticAnalysisError):
    pass


class ConstraintsError(StaticAnalysisError):
    """The constraints of a routine (inputs, outputs, trashes) have been violated."""
    pass


class ConstantConstraintError(ConstraintsError):
    pass


class InconsistentConstraintsError(ConstraintsError):
    pass


class IncompatibleConstraintsError(ConstraintsError):
    pass


class Analyzer(object):

    def __init__(self, symtab, debug=False):
        self.symtab = symtab
        self.current_routine = None
        self.debug = debug
        self.exit_contexts_map = {}

    # - - - - helper methods - - - -

    def get_type_for_name(self, name):
        if self.current_routine and self.symtab.has_local(self.current_routine.name, name):
            return self.symtab.fetch_local_type(self.current_routine.name, name)
        return self.symtab.fetch_global_type(name)

    def get_type(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.type
        if not isinstance(ref, LocationRef):
            raise NotImplementedError(str(ref))
        return self.get_type_for_name(ref.name)

    def assert_type(self, type_, *locations):
        for location in locations:
            if self.get_type(location) != type_:
                raise TypeMismatchError(self.current_routine, location.name)

    def assert_affected_within(self, name, affecting_type, limiting_type):
        assert name in ('inputs', 'outputs', 'trashes')
        affected = getattr(affecting_type, name)
        limited_to = getattr(limiting_type, name)
        overage = affected - limited_to
        if not overage:
            return
        message = '%s for %s are %s\n\nbut %s affects %s\n\nwhich exceeds it by: %s ' % (
            name,
            limiting_type, LocationRef.format_set(limited_to),
            affecting_type, LocationRef.format_set(affected),
            LocationRef.format_set(overage)
        )
        raise IncompatibleConstraintsError(self.current_routine, message)

    def assert_types_for_read_table(self, context, instr, src, dest, type_, offset):
        if (not TableType.is_a_table_type(self.get_type(src.ref), type_)) or (not self.get_type(dest) == type_):
            raise TypeMismatchError(instr, '{} and {}'.format(src.ref.name, dest.name))
        context.assert_meaningful(src, src.index)
        context.assert_in_range(src.index, src.ref, offset)

    def assert_types_for_update_table(self, context, instr, dest, type_, offset):
        if not TableType.is_a_table_type(self.get_type(dest.ref), type_):
            raise TypeMismatchError(instr, '{}'.format(dest.ref.name))
        context.assert_meaningful(dest.index)
        context.assert_in_range(dest.index, dest.ref, offset)
        context.set_written(dest.ref)

    # - - - - visitor methods - - - -

    def analyze_program(self, program):
        assert isinstance(program, Program)
        for routine in program.routines:
            context, type_ = self.analyze_routine(routine)
            if type_:
                routine.routine_type = type_
            routine.encountered_gotos = list(context.encountered_gotos()) if context else []
            routine.called_routines = list(context.called_routines) if context else []

    def analyze_routine(self, routine):
        assert isinstance(routine, Routine)
        type_ = self.get_type_for_name(routine.name)

        if routine.block is None:
            # it's an extern, that's fine
            return None, type_

        self.current_routine = routine

        context = AnalysisContext(self.symtab, routine, type_.inputs, type_.outputs, type_.trashes)

        # register any local statics as already-initialized
        for local_name, local_symentry in self.symtab.locals.get(routine.name, {}).items():
            ref = self.symtab.fetch_local_ref(routine.name, local_name)
            if local_symentry.ast_node.initial is not None:
                context.set_meaningful(ref)
                context.set_range(ref, local_symentry.ast_node.initial, local_symentry.ast_node.initial)

        self.exit_contexts = []

        self.analyze_block(routine.block, context)

        trashed = set(context.each_touched()) - set(context.each_meaningful())

        self.exit_contexts_map[routine.name] = {
            'end_context': context.to_json_data(),
            'exit_contexts': [e.to_json_data() for e in self.exit_contexts]
        }

        if self.exit_contexts:
            # check that they are all consistent
            exit_context = self.exit_contexts[0]
            exit_meaningful = set(exit_context.each_meaningful())
            exit_touched = set(exit_context.each_touched())
            exit_writeable = set(exit_context.each_writeable())
            for ex in self.exit_contexts[1:]:
                if set(ex.each_meaningful()) != exit_meaningful:
                    raise InconsistentExitError(routine, "Exit contexts are not consistent")
                if set(ex.each_touched()) != exit_touched:
                    raise InconsistentExitError(routine, "Exit contexts are not consistent")
                if set(ex.each_writeable()) != exit_writeable:
                    raise InconsistentExitError(routine, "Exit contexts are not consistent")

            # We now set the main context to the (consistent) exit context
            # so that this routine is perceived as having the same effect
            # that any of the goto'ed routines have.
            context.update_from(exit_context)

        # these all apply whether we encountered goto(s) in this routine, or not...:

        # can't trash an output.
        for ref in trashed:
            if ref in type_.outputs:
                raise UnmeaningfulOutputError(routine, ref.name)

        # all outputs are meaningful.
        for ref in type_.outputs:
            context.assert_meaningful(ref, exception_class=UnmeaningfulOutputError)

        # if something was touched, then it should have been declared to be writable.
        for ref in context.each_touched():
            if ref not in type_.outputs and ref not in type_.trashes and not self.symtab.has_local(routine.name, ref.name):
                raise ForbiddenWriteError(routine, ref.name)

        self.exit_contexts = None
        self.current_routine = None
        return context, type_

    def analyze_block(self, block, context):
        assert isinstance(block, Block)
        for i in block.instrs:
            self.analyze_instr(i, context)

    def analyze_instr(self, instr, context):
        if isinstance(instr, SingleOp):
            self.analyze_single_op(instr, context)
        elif isinstance(instr, Call):
            self.analyze_call(instr, context)
        elif isinstance(instr, GoTo):
            self.analyze_goto(instr, context)
        elif isinstance(instr, If):
            self.analyze_if(instr, context)
        elif isinstance(instr, Repeat):
            self.analyze_repeat(instr, context)
        elif isinstance(instr, For):
            self.analyze_for(instr, context)
        elif isinstance(instr, WithInterruptsOff):
            self.analyze_with_interrupts_off(instr, context)
        elif isinstance(instr, Save):
            self.analyze_save(instr, context)
        elif isinstance(instr, PointInto):
            self.analyze_point_into(instr, context)
        elif isinstance(instr, Reset):
            self.analyze_reset(instr, context)
        else:
            raise NotImplementedError(str(instr))

    def analyze_single_op(self, instr, context):

        opcode = instr.opcode
        dest = instr.dest
        src = instr.src

        if context.has_terminated():
            raise TerminatedContextError(instr, instr)

        if opcode == 'ld':
            if isinstance(src, IndexedRef):
                self.assert_types_for_read_table(context, instr, src, dest, TYPE_BYTE, src.offset)
            elif isinstance(src, IndirectRef):
                # copying this analysis from the matching branch in `copy`, below
                if isinstance(self.get_type(src.ref), PointerType) and self.get_type(dest) == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))

                origin = context.get_assoc(src.ref)
                if not origin:
                    raise UnmeaningfulReadError(instr, src.ref)
                context.assert_meaningful(origin)

                context.assert_meaningful(src.ref, REG_Y)
            elif self.get_type(src) != self.get_type(dest):
                raise TypeMismatchError(instr, '{} and {}'.format(src.name, dest.name))
            else:
                context.assert_meaningful(src)
                context.copy_range(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode == 'st':
            if isinstance(dest, IndexedRef):
                if self.get_type(src) != TYPE_BYTE:
                    raise TypeMismatchError(instr, (src, dest))
                self.assert_types_for_update_table(context, instr, dest, TYPE_BYTE, dest.offset)
            elif isinstance(dest, IndirectRef):
                # copying this analysis from the matching branch in `copy`, below
                if isinstance(self.get_type(dest.ref), PointerType) and self.get_type(src) == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))

                context.assert_meaningful(dest.ref, REG_Y)

                target = context.get_assoc(dest.ref)
                if not target:
                    raise ForbiddenWriteError(instr, dest.ref)
                context.set_written(target)

            elif self.get_type(src) != self.get_type(dest):
                raise TypeMismatchError(instr, '{} and {}'.format(src, dest))
            else:
                context.set_written(dest)
                # FIXME: context.copy_range(src, dest)   ?
            context.assert_meaningful(src)
        elif opcode == 'add':
            context.assert_meaningful(src, dest, FLAG_C)
            if isinstance(src, IndexedRef):
                self.assert_types_for_read_table(context, instr, src, dest, TYPE_BYTE, src.offset)
            elif self.get_type(src) == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
                if dest != REG_A:
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
            else:
                self.assert_type(TYPE_WORD, src)
                dest_type = self.get_type(dest)
                if dest_type == TYPE_WORD:
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
                elif isinstance(dest_type, PointerType):
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
                else:
                    self.assert_type(TYPE_WORD, dest)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
            context.invalidate_range(dest)
        elif opcode == 'sub':
            context.assert_meaningful(src, dest, FLAG_C)
            if isinstance(src, IndexedRef):
                self.assert_types_for_read_table(context, instr, src, dest, TYPE_BYTE, src.offset)
            elif self.get_type(src) == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
                if dest != REG_A:
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
            else:
                self.assert_type(TYPE_WORD, src, dest)
                context.set_touched(REG_A)
                context.set_unmeaningful(REG_A)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
            context.invalidate_range(dest)
        elif opcode == 'cmp':
            context.assert_meaningful(src, dest)
            if isinstance(src, IndexedRef):
                self.assert_types_for_read_table(context, instr, src, dest, TYPE_BYTE, src.offset)
            elif self.get_type(src) == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
            else:
                self.assert_type(TYPE_WORD, src, dest)
                context.set_touched(REG_A)
                context.set_unmeaningful(REG_A)
            context.set_written(FLAG_Z, FLAG_N, FLAG_C)
        elif opcode == 'and':
            if isinstance(src, IndexedRef):
                self.assert_types_for_read_table(context, instr, src, dest, TYPE_BYTE, src.offset)
            else:
                self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
            # If you AND the A register with a value V, the resulting value of A
            # cannot exceed the value of V; i.e. the maximum value of A becomes
            # the maximum value of V.
            if not isinstance(src, IndexedRef):
                context.set_top_of_range(dest, context.get_top_of_range(src))
        elif opcode in ('or', 'xor'):
            if isinstance(src, IndexedRef):
                self.assert_types_for_read_table(context, instr, src, dest, TYPE_BYTE, src.offset)
            else:
                self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
            context.invalidate_range(dest)
        elif opcode in ('inc', 'dec'):
            context.assert_meaningful(dest)
            if isinstance(dest, IndexedRef):
                self.assert_types_for_update_table(context, instr, dest, TYPE_BYTE, dest.offset)
                context.set_written(dest.ref, FLAG_Z, FLAG_N)
                #context.invalidate_range(dest)
            else:
                self.assert_type(TYPE_BYTE, dest)
                context.set_written(dest, FLAG_Z, FLAG_N)
                bottom = context.get_bottom_of_range(dest)
                top = context.get_top_of_range(dest)
                if opcode == 'inc':
                    if bottom == top and top < 255:
                        context.set_range(dest, bottom + 1, top + 1)
                    else:
                        context.invalidate_range(dest)
                elif opcode == 'dec':
                    if bottom == top and bottom > 0:
                        context.set_range(dest, bottom - 1, top - 1)
                    else:
                        context.invalidate_range(dest)
                else:
                    raise NotImplementedError
        elif opcode in ('shl', 'shr'):
            context.assert_meaningful(dest, FLAG_C)
            if isinstance(dest, IndexedRef):
                self.assert_types_for_update_table(context, instr, dest, TYPE_BYTE, dest.offset)
                context.set_written(dest.ref, FLAG_Z, FLAG_N, FLAG_C)
                #context.invalidate_range(dest)
            else:
                self.assert_type(TYPE_BYTE, dest)
                context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C)
                context.invalidate_range(dest)
        elif opcode == 'copy':
            if dest == REG_A:
                raise ForbiddenWriteError(instr, "{} cannot be used as destination for copy".format(dest))

            # 1. check that their types are compatible

            if isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndirectRef):
                if self.get_type(src) == TYPE_BYTE and isinstance(self.get_type(dest.ref), PointerType):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                if isinstance(self.get_type(src.ref), PointerType) and self.get_type(dest) == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
            elif isinstance(src, IndirectRef) and isinstance(dest, IndirectRef):
                if isinstance(self.get_type(src.ref), PointerType) and isinstance(self.get_type(dest.ref), PointerType):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))

            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndexedRef):
                if self.get_type(src) == TYPE_WORD and TableType.is_a_table_type(self.get_type(dest.ref), TYPE_WORD):
                    pass
                elif (isinstance(self.get_type(src), VectorType) and isinstance(self.get_type(dest.ref), TableType) and
                      RoutineType.executable_types_compatible(self.get_type(src).of_type, self.get_type(dest.ref).of_type)):
                    pass
                elif (isinstance(self.get_type(src), RoutineType) and isinstance(self.get_type(dest.ref), TableType) and
                      RoutineType.executable_types_compatible(self.get_type(src), self.get_type(dest.ref).of_type)):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_in_range(dest.index, dest.ref, dest.offset)

            elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef):
                if TableType.is_a_table_type(self.get_type(src.ref), TYPE_WORD) and self.get_type(dest) == TYPE_WORD:
                    pass
                elif (isinstance(self.get_type(src.ref), TableType) and isinstance(self.get_type(dest), VectorType) and
                      RoutineType.executable_types_compatible(self.get_type(src.ref).of_type, self.get_type(dest).of_type)):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_in_range(src.index, src.ref, src.offset)

            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, LocationRef):
                if self.get_type(src) == self.get_type(dest):
                    pass
                elif isinstance(self.get_type(src), RoutineType) and isinstance(self.get_type(dest), VectorType):
                    self.assert_affected_within('inputs', self.get_type(src), self.get_type(dest).of_type)
                    self.assert_affected_within('outputs', self.get_type(src), self.get_type(dest).of_type)
                    self.assert_affected_within('trashes', self.get_type(src), self.get_type(dest).of_type)
                else:
                    raise TypeMismatchError(instr, (src, dest))
            else:
                raise TypeMismatchError(instr, (src, dest))

            # 2. check that the context is meaningful

            if isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndirectRef):
                context.assert_meaningful(src, dest.ref, REG_Y)

                target = context.get_assoc(dest.ref)
                if not target:
                    raise ForbiddenWriteError(instr, dest.ref)
                context.set_written(target)

            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                context.assert_meaningful(src.ref, REG_Y)

                origin = context.get_assoc(src.ref)
                if not origin:
                    raise UnmeaningfulReadError(instr, src.ref)
                context.assert_meaningful(origin)

                context.set_written(dest)

            elif isinstance(src, IndirectRef) and isinstance(dest, IndirectRef):
                context.assert_meaningful(src.ref, dest.ref, REG_Y)

                origin = context.get_assoc(src.ref)
                if not origin:
                    raise UnmeaningfulReadError(instr, src.ref)
                context.assert_meaningful(origin)

                target = context.get_assoc(dest.ref)
                if not target:
                    raise ForbiddenWriteError(instr, dest.ref)
                context.set_written(target)

            elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef):
                context.assert_meaningful(src, dest.ref, dest.index)
                context.set_written(dest.ref)
            elif isinstance(src, ConstantRef) and isinstance(dest, IndexedRef):
                context.assert_meaningful(src, dest.ref, dest.index)
                context.set_written(dest.ref)
            elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef):
                context.assert_meaningful(src.ref, src.index)
                context.set_written(dest)
            else:
                context.assert_meaningful(src)
                context.set_written(dest)

            context.set_touched(REG_A, FLAG_Z, FLAG_N)
            context.set_unmeaningful(REG_A, FLAG_Z, FLAG_N)

        elif opcode == 'trash':
            context.set_touched(instr.dest)
            context.set_unmeaningful(instr.dest)
        elif opcode == 'nop':
            pass
        else:
            raise NotImplementedError(opcode)

    def analyze_call(self, instr, context):
        type_ = self.get_type(instr.location)
        if not isinstance(type_, (RoutineType, VectorType)):
            raise TypeMismatchError(instr, instr.location.name)
        context.mark_as_called(instr.location, type_)
        if isinstance(type_, VectorType):
            type_ = type_.of_type
        for ref in type_.inputs:
            context.assert_meaningful(ref)
        for ref in type_.outputs:
            context.set_written(ref)
        for ref in type_.trashes:
            context.assert_writeable(ref)
            context.set_touched(ref)
            context.set_unmeaningful(ref)

    def analyze_goto(self, instr, context):
        location = instr.location
        type_ = self.get_type(instr.location)

        if not isinstance(type_, (RoutineType, VectorType)):
            raise TypeMismatchError(instr, location.name)
        context.mark_as_called(instr.location, type_)

        # assert that the dest routine's inputs are all initialized
        if isinstance(type_, VectorType):
            type_ = type_.of_type
        for ref in type_.inputs:
            context.assert_meaningful(ref)

        # and that this routine's trashes and output constraints are a
        # superset of the called routine's
        current_type = self.get_type_for_name(self.current_routine.name)
        self.assert_affected_within('outputs', type_, current_type)
        self.assert_affected_within('trashes', type_, current_type)

        context.encounter_gotos(set([instr.location]))

        # Now that we have encountered a goto, we update the
        # context here to match what someone calling the goto'ed
        # function directly, would expect.  (which makes sense
        # when you think about it; if this goto's F, then calling
        # this is like calling F, from the perspective of what is
        # returned.)
        #
        # However, this isn't the current context anymore.  This
        # is an exit context of this routine.

        exit_context = context.clone()

        for ref in type_.outputs:
            exit_context.set_written(ref)

        for ref in type_.trashes:
            exit_context.assert_writeable(ref)
            exit_context.set_touched(ref)
            exit_context.set_unmeaningful(ref)

        self.exit_contexts.append(exit_context)

        # When we get to the end, we'll check that all the
        # exit contexts are consistent with each other.

        # We set the current context as having terminated.
        # If we are in a branch, the merge will deal with
        # having terminated.  If we are at the end of the
        # routine, the routine end will deal with that.

        context.set_terminated()

    def analyze_if(self, instr, context):
        incoming_meaningful = set(context.each_meaningful())

        context1 = context.clone()
        context2 = context.clone()
        self.analyze_block(instr.block1, context1)
        if instr.block2 is not None:
            self.analyze_block(instr.block2, context2)

        outgoing_meaningful = set(context1.each_meaningful()) & set(context2.each_meaningful())
        outgoing_trashes = incoming_meaningful - outgoing_meaningful

        # merge the contexts.

        # first, the easy case: if one of the contexts has terminated, just use the other one.
        # if both have terminated, we return a terminated context, and that's OK.

        if context1.has_terminated():
            context.update_from(context2)
        elif context2.has_terminated():
            context.update_from(context1)
        else:
            # the more complicated case: merge the contents of the contexts.
            context._touched = set(context1._touched) | set(context2._touched)
            context.set_meaningful(*list(outgoing_meaningful))
            context._writeable = set(context1._writeable) | set(context2._writeable)

        # in both cases, we need to merge the encountered gotos, in order that
        # fallthru optimization continues to work correctly.
        context.encounter_gotos(context1.encountered_gotos() | context2.encountered_gotos())

        for ref in outgoing_trashes:
            context.set_touched(ref)
            context.set_unmeaningful(ref)

    def analyze_repeat(self, instr, context):
        # it will always be executed at least once, so analyze it having
        # been executed the first time.
        self.analyze_block(instr.block, context)
        if instr.src is not None:  # None indicates 'repeat forever'
            context.assert_meaningful(instr.src)

        if context.encountered_gotos():
            raise IllegalJumpError(instr, instr)

        # now analyze it having been executed a second time, with the context
        # of it having already been executed.
        self.analyze_block(instr.block, context)
        if instr.src is not None:
            context.assert_meaningful(instr.src)

    def analyze_for(self, instr, context):
        context.assert_meaningful(instr.dest)
        context.assert_writeable(instr.dest)

        bottom, top = context.get_range(instr.dest)
        final = instr.final.value

        if instr.direction > 0:
            if top >= final:
                raise RangeExceededError(instr, "Top of range of {} is {} but must be lower than {}".format(
                    instr.dest, top, final
                ))
            top = final

        if instr.direction < 0:
            if bottom <= final:
                raise RangeExceededError(instr, "Bottom of range of {} is {} but must be higher than {}".format(
                    instr.dest, bottom, final
                ))
            bottom = final

        # inside the block, the loop variable cannot be modified, and we know its range.
        context.set_range(instr.dest, bottom, top)
        context.set_unwriteable(instr.dest)

        # it will always be executed at least once, so analyze it having
        # been executed the first time.
        self.analyze_block(instr.block, context)

        # now analyze it having been executed a second time, with the context
        # of it having already been executed.
        self.analyze_block(instr.block, context)

        # after it is executed, we know the range of the loop variable.
        context.set_range(instr.dest, instr.final, instr.final)
        context.set_writeable(instr.dest)

    def analyze_with_interrupts_off(self, instr, context):
        block = instr.block
        for instr in block.instrs:
            if isinstance(instr, (Call, GoTo, WithInterruptsOff)):
                raise IllegalJumpError(instr, instr)
            self.analyze_instr(instr, context)

    def analyze_save(self, instr, context):
        batons = []
        for location in instr.locations:
            self.assert_type(TYPE_BYTE, location)
            baton = context.extract(location)
            batons.append(baton)

        self.analyze_block(instr.block, context)
        if context.encountered_gotos():
            raise IllegalJumpError(instr, instr)

        for location in reversed(instr.locations):
            baton = batons.pop()
            context.re_introduce(baton)

        # We do this check outside the loop, because A is only preserved
        # if it is the outermost thing being `save`d.
        if location == REG_A:
            pass
        else:
            context.set_touched(REG_A)
            context.set_unmeaningful(REG_A)

    def analyze_point_into(self, instr, context):
        if not isinstance(self.get_type(instr.pointer), PointerType):
            raise TypeMismatchError(instr, instr.pointer)
        if not TableType.is_a_table_type(self.get_type(instr.table), TYPE_BYTE):
            raise TypeMismatchError(instr, instr.table)

        # check that pointer is not yet associated with any table.

        if context.get_assoc(instr.pointer):
            raise ForbiddenWriteError(instr, instr.pointer)

        # associate pointer with table
        # (do not mark it as meaningful yet - that's reset's job.)

        context.set_assoc(instr.pointer, instr.table)
        context.set_unmeaningful(instr.pointer)

        self.analyze_block(instr.block, context)
        if context.encountered_gotos():
            raise IllegalJumpError(instr, instr)

        # unassociate pointer with table, mark as unmeaningful.

        context.set_assoc(instr.pointer, None)
        context.set_unmeaningful(instr.pointer)

    def analyze_reset(self, instr, context):
        type = self.get_type(instr.pointer)
        if not isinstance(type, (PointerType)):
            raise TypeMismatchError(instr, instr.pointer.name)

        table = context.get_assoc(instr.pointer)
        if not table:
            raise ForbiddenWriteError(instr, '{} is not associated with any table'.format(instr.pointer.name))
        context.assert_meaningful(table)
        low_limit, high_limit = context.get_range(table)

        assert isinstance(instr.offset, ConstantRef)
        if instr.offset.value < low_limit or instr.offset.value > high_limit:
            raise RangeExceededError(instr, instr.pointer.name)

        context.set_meaningful(instr.pointer)
        context.set_touched(instr.pointer)
