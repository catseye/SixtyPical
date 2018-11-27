# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, SingleOp, If, Repeat, For, WithInterruptsOff, Save
from sixtypical.model import (
    TYPE_BYTE, TYPE_WORD,
    TableType, BufferType, PointerType, VectorType, RoutineType,
    ConstantRef, LocationRef, IndirectRef, IndexedRef, AddressRef,
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


class ForbiddenWriteError(StaticAnalysisError):
    pass


class TypeMismatchError(StaticAnalysisError):
    pass


class IllegalJumpError(StaticAnalysisError):
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


def routine_has_static(routine, ref):
    if not hasattr(routine, 'statics'):
        return False
    for static in routine.statics:
        if static.location == ref:
            return True
    return False


class Context(object):
    """
    A location is touched if it was changed (or even potentially
    changed) during this routine, or some routine called by this routine.

    A location is meaningful if it was an input to this routine,
    or if it was set to a meaningful value by some operation in this
    routine (or some routine called by this routine).

    If a location is meaningful, it has a range.  This range represents
    the lowest and highest values that it might possibly be (i.e. we know
    it cannot possibly be below the lowest or above the highest.)  In the
    absence of any usage information, the range of a byte, is 0..255 and
    the range of a word is 0..65535.

    A location is writeable if it was listed in the outputs and trashes
    lists of this routine.  A location can also be temporarily marked
    unwriteable in certain contexts, such as `for` loops.
    """
    def __init__(self, routines, routine, inputs, outputs, trashes):
        self.routines = routines    # Location -> AST node
        self.routine = routine
        self._touched = set()
        self._range = dict()
        self._writeable = set()
        self._gotos_encountered = set()

        for ref in inputs:
            if ref.is_constant():
                raise ConstantConstraintError(self.routine, ref.name)
            self._range[ref] = ref.max_range()
        output_names = set()
        for ref in outputs:
            if ref.is_constant():
                raise ConstantConstraintError(self.routine, ref.name)
            output_names.add(ref.name)
            self._writeable.add(ref)
        for ref in trashes:
            if ref.is_constant():
                raise ConstantConstraintError(self.routine, ref.name)
            if ref.name in output_names:
                raise InconsistentConstraintsError(self.routine, ref.name)
            self._writeable.add(ref)

    def __str__(self):
        return "Context(\n  _touched={},\n  _range={},\n  _writeable={}\n)".format(
            LocationRef.format_set(self._touched), LocationRef.format_set(self._range), LocationRef.format_set(self._writeable)
        )

    def clone(self):
        c = Context(self.routines, self.routine, [], [], [])
        c._touched = set(self._touched)
        c._range = dict(self._range)
        c._writeable = set(self._writeable)
        return c

    def each_meaningful(self):
        for ref in self._range.keys():
            yield ref

    def each_touched(self):
        for ref in self._touched:
            yield ref

    def assert_meaningful(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', UnmeaningfulReadError)
        for ref in refs:
            # statics are always meaningful
            if routine_has_static(self.routine, ref):
                continue
            if ref.is_constant() or ref in self.routines:
                pass
            elif isinstance(ref, LocationRef):
                if ref not in self._range:
                    message = ref.name
                    if kwargs.get('message'):
                        message += ' (%s)' % kwargs['message']
                    raise exception_class(self.routine, message)
            elif isinstance(ref, IndexedRef):
                self.assert_meaningful(ref.ref, **kwargs)
                self.assert_meaningful(ref.index, **kwargs)
            else:
                raise NotImplementedError(ref)

    def assert_writeable(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', ForbiddenWriteError)
        for ref in refs:
            # statics are always writeable
            if routine_has_static(self.routine, ref):
                continue
            if ref not in self._writeable:
                message = ref.name
                if kwargs.get('message'):
                    message += ' (%s)' % kwargs['message']
                raise exception_class(self.routine, message)

    def assert_in_range(self, inside, outside):
        # FIXME there's a bit of I'm-not-sure-the-best-way-to-do-this-ness, here...

        # inside should always be meaningful
        inside_range = self._range[inside]

        # outside might not be meaningful, so default to max range if necessary
        if outside in self._range:
            outside_range = self._range[outside]
        else:
            outside_range = outside.max_range()
        if isinstance(outside.type, TableType):
            outside_range = (0, outside.type.size-1)

        if inside_range[0] < outside_range[0] or inside_range[1] > outside_range[1]:
            raise RangeExceededError(self.routine,
                "Possible range of {} {} exceeds acceptable range of {} {}".format(
                    inside, inside_range, outside, outside_range
                )
            )

    def set_touched(self, *refs):
        for ref in refs:
            self._touched.add(ref)
            # TODO: it might be possible to invalidate the range here

    def set_meaningful(self, *refs):
        for ref in refs:
            if ref not in self._range:
                self._range[ref] = ref.max_range()

    def set_top_of_range(self, ref, top):
        self.assert_meaningful(ref)
        (bottom, _) = self._range[ref]
        self._range[ref] = (bottom, top)

    def set_bottom_of_range(self, ref, bottom):
        self.assert_meaningful(ref)
        (top, _) = self._range[ref]
        self._range[ref] = (bottom, top)

    def set_range(self, ref, bottom, top):
        self.assert_meaningful(ref)
        self._range[ref] = (bottom, top)

    def get_top_of_range(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        self.assert_meaningful(ref)
        (_, top) = self._range[ref]
        return top

    def get_bottom_of_range(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        self.assert_meaningful(ref)
        (bottom, _) = self._range[ref]
        return bottom

    def get_range(self, ref):
        if isinstance(ref, ConstantRef):
            return (ref.value, ref.value)
        self.assert_meaningful(ref)
        (bottom, top) = self._range[ref]
        return bottom, top

    def copy_range(self, src, dest):
        self.assert_meaningful(src)
        if src in self._range:
            src_range = self._range[src]
        else:
            src_range = src.max_range()
        self._range[dest] = src_range

    def invalidate_range(self, ref):
        self.assert_meaningful(ref)
        self._range[ref] = ref.max_range()

    def set_unmeaningful(self, *refs):
        for ref in refs:
            if ref in self._range:
                del self._range[ref]

    def set_written(self, *refs):
        """A "helper" method which does the following common sequence for
        the given refs: asserts they're all writable, and sets them all
        as touched and meaningful."""
        self.assert_writeable(*refs)
        self.set_touched(*refs)
        self.set_meaningful(*refs)

    def set_unwriteable(self, *refs):
        """Intended to be used for implementing analyzing `for`."""
        for ref in refs:
            self._writeable.remove(ref)

    def set_writeable(self, *refs):
        """Intended to be used for implementing analyzing `for`, but also used in `save`."""
        for ref in refs:
            self._writeable.add(ref)

    def encounter_gotos(self, gotos):
        self._gotos_encountered |= gotos

    def encountered_gotos(self):
        return self._gotos_encountered

    def assert_types_for_read_table(self, instr, src, dest, type_):
        if (not TableType.is_a_table_type(src.ref.type, type_)) or (not dest.type == type_):
            raise TypeMismatchError(instr, '{} and {}'.format(src.ref.name, dest.name))
        self.assert_meaningful(src, src.index)
        self.assert_in_range(src.index, src.ref)

    def assert_types_for_update_table(self, instr, dest, type_):
        if not TableType.is_a_table_type(dest.ref.type, type_):
            raise TypeMismatchError(instr, '{}'.format(dest.ref.name))
        self.assert_meaningful(dest.index)
        self.assert_in_range(dest.index, dest.ref)
        self.set_written(dest.ref)

    def extract(self, location):
        """Sets the given location as writeable in the context, and returns a 'baton' representing
        the previous state of context for that location.  This 'baton' can be used to later restore
        this state of context."""
        # Used in `save`.
        baton = (
            location,
            location in self._touched,
            self._range.get(location, None),
            location in self._writeable,
        )
        self.set_writeable(location)
        return baton

    def re_introduce(self, baton):
        """Given a 'baton' produced by `extract()`, restores the context for that saved location
        to what it was before `extract()` was called."""
        # Used in `save`.
        location, was_touched, was_range, was_writeable = baton

        if was_touched:
            self._touched.add(location)
        elif location in self._touched:
            self._touched.remove(location)

        if was_range is not None:
            self._range[location] = was_range
        elif location in self._range:
            del self._range[location]

        if was_writeable:
            self._writeable.add(location)
        elif location in self._writeable:
            self._writeable.remove(location)


class Analyzer(object):

    def __init__(self, debug=False):
        self.current_routine = None
        self.routines = {}
        self.debug = debug

    def assert_type(self, type_, *locations):
        for location in locations:
            if location.type != type_:
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

    def analyze_program(self, program):
        assert isinstance(program, Program)
        self.routines = {r.location: r for r in program.routines}
        for routine in program.routines:
            context = self.analyze_routine(routine)
            routine.encountered_gotos = list(context.encountered_gotos()) if context else []

    def analyze_routine(self, routine):
        assert isinstance(routine, Routine)
        self.current_routine = routine
        if routine.block is None:
            # it's an extern, that's fine
            return
        type_ = routine.location.type
        context = Context(self.routines, routine, type_.inputs, type_.outputs, type_.trashes)

        if self.debug:
            print("at start of routine `{}`:".format(routine.name))
            print(context)

        self.analyze_block(routine.block, context)
        trashed = set(context.each_touched()) - set(context.each_meaningful())

        if self.debug:
            print("at end of routine `{}`:".format(routine.name))
            print(context)
            print("trashed: ", LocationRef.format_set(trashed))
            print("outputs: ", LocationRef.format_set(type_.outputs))
            trashed_outputs = type_.outputs & trashed
            if trashed_outputs:
                print("TRASHED OUTPUTS: ", LocationRef.format_set(trashed_outputs))
            print('')
            print('-' * 79)
            print('')

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
            if ref not in type_.outputs and ref not in type_.trashes and not routine_has_static(routine, ref):
                raise ForbiddenWriteError(routine, ref.name)

        self.current_routine = None
        return context

    def analyze_block(self, block, context):
        assert isinstance(block, Block)
        for i in block.instrs:
            self.analyze_instr(i, context)

    def analyze_instr(self, instr, context):
        if isinstance(instr, SingleOp):
            self.analyze_single_op(instr, context)
        elif isinstance(instr, If):
            self.analyze_if(instr, context)
        elif isinstance(instr, Repeat):
            self.analyze_repeat(instr, context)
        elif isinstance(instr, For):
            self.analyze_for(instr, context)
        elif isinstance(instr, WithInterruptsOff):
            self.analyze_block(instr.block, context)
            if context.encountered_gotos():
                raise IllegalJumpError(instr, instr)
        elif isinstance(instr, Save):
            self.analyze_save(instr, context)
        else:
            raise NotImplementedError

    def analyze_single_op(self, instr, context):

        opcode = instr.opcode
        dest = instr.dest
        src = instr.src

        if opcode == 'ld':
            if isinstance(src, IndexedRef):
                context.assert_types_for_read_table(instr, src, dest, TYPE_BYTE)
            elif isinstance(src, IndirectRef):
                # copying this analysis from the matching branch in `copy`, below
                if isinstance(src.ref.type, PointerType) and dest.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_meaningful(src.ref, REG_Y)
            elif src.type != dest.type:
                raise TypeMismatchError(instr, '{} and {}'.format(src.name, dest.name))
            else:
                context.assert_meaningful(src)
                context.copy_range(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode == 'st':
            if isinstance(dest, IndexedRef):
                if src.type != TYPE_BYTE:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_types_for_update_table(instr, dest, TYPE_BYTE)
            elif isinstance(dest, IndirectRef):
                # copying this analysis from the matching branch in `copy`, below
                if isinstance(dest.ref.type, PointerType) and src.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_meaningful(dest.ref, REG_Y)
                context.set_written(dest.ref)
            elif src.type != dest.type:
                raise TypeMismatchError(instr, '{} and {}'.format(src, dest))
            else:
                context.set_written(dest)
                # FIXME: context.copy_range(src, dest)   ?
            context.assert_meaningful(src)
        elif opcode == 'add':
            context.assert_meaningful(src, dest, FLAG_C)
            if isinstance(src, IndexedRef):
                context.assert_types_for_read_table(instr, src, dest, TYPE_BYTE)
            elif src.type == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
            else:
                self.assert_type(TYPE_WORD, src)
                if dest.type == TYPE_WORD:
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
                elif isinstance(dest.type, PointerType):
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
                else:
                    self.assert_type(TYPE_WORD, dest)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
            context.invalidate_range(dest)
        elif opcode == 'sub':
            context.assert_meaningful(src, dest, FLAG_C)
            if isinstance(src, IndexedRef):
                context.assert_types_for_read_table(instr, src, dest, TYPE_BYTE)
            elif src.type == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
            else:
                self.assert_type(TYPE_WORD, src, dest)
                context.set_touched(REG_A)
                context.set_unmeaningful(REG_A)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
            context.invalidate_range(dest)
        elif opcode == 'cmp':
            context.assert_meaningful(src, dest)
            if isinstance(src, IndexedRef):
                context.assert_types_for_read_table(instr, src, dest, TYPE_BYTE)
            else:
                self.assert_type(TYPE_BYTE, src, dest)
            context.set_written(FLAG_Z, FLAG_N, FLAG_C)
        elif opcode == 'and':
            if isinstance(src, IndexedRef):
                context.assert_types_for_read_table(instr, src, dest, TYPE_BYTE)
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
                context.assert_types_for_read_table(instr, src, dest, TYPE_BYTE)
            else:
                self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
            context.invalidate_range(dest)
        elif opcode in ('inc', 'dec'):
            context.assert_meaningful(dest)
            if isinstance(dest, IndexedRef):
                context.assert_types_for_update_table(instr, dest, TYPE_BYTE)
                context.set_written(dest.ref, FLAG_Z, FLAG_N)
                #context.invalidate_range(dest)
            else:
                self.assert_type(TYPE_BYTE, dest)
                context.set_written(dest, FLAG_Z, FLAG_N)
                context.invalidate_range(dest)
        elif opcode in ('shl', 'shr'):
            context.assert_meaningful(dest, FLAG_C)
            if isinstance(dest, IndexedRef):
                context.assert_types_for_update_table(instr, dest, TYPE_BYTE)
                context.set_written(dest.ref, FLAG_Z, FLAG_N, FLAG_C)
                #context.invalidate_range(dest)
            else:
                self.assert_type(TYPE_BYTE, dest)
                context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C)
                context.invalidate_range(dest)
        elif opcode == 'call':
            type = instr.location.type
            if not isinstance(type, (RoutineType, VectorType)):
                raise TypeMismatchError(instr, instr.location)
            if isinstance(type, VectorType):
                type = type.of_type
            for ref in type.inputs:
                context.assert_meaningful(ref)
            for ref in type.outputs:
                context.set_written(ref)
            for ref in type.trashes:
                context.assert_writeable(ref)
                context.set_touched(ref)
                context.set_unmeaningful(ref)
        elif opcode == 'copy':
            if dest == REG_A:
                raise ForbiddenWriteError(instr, "{} cannot be used as destination for copy".format(dest))

            # 1. check that their types are compatible

            if isinstance(src, AddressRef) and isinstance(dest, LocationRef):
                if isinstance(src.ref.type, BufferType) and isinstance(dest.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndirectRef):
                if src.type == TYPE_BYTE and isinstance(dest.ref.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                if isinstance(src.ref.type, PointerType) and dest.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
            elif isinstance(src, IndirectRef) and isinstance(dest, IndirectRef):
                if isinstance(src.ref.type, PointerType) and isinstance(dest.ref.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))

            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndexedRef):
                if src.type == TYPE_WORD and TableType.is_a_table_type(dest.ref.type, TYPE_WORD):
                    pass
                elif (isinstance(src.type, VectorType) and isinstance(dest.ref.type, TableType) and
                      RoutineType.executable_types_compatible(src.type.of_type, dest.ref.type.of_type)):
                    pass
                elif (isinstance(src.type, RoutineType) and isinstance(dest.ref.type, TableType) and
                      RoutineType.executable_types_compatible(src.type, dest.ref.type.of_type)):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_in_range(dest.index, dest.ref)

            elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef):
                if TableType.is_a_table_type(src.ref.type, TYPE_WORD) and dest.type == TYPE_WORD:
                    pass
                elif (isinstance(src.ref.type, TableType) and isinstance(dest.type, VectorType) and
                      RoutineType.executable_types_compatible(src.ref.type.of_type, dest.type.of_type)):
                    pass
                else:
                    raise TypeMismatchError(instr, (src, dest))
                context.assert_in_range(src.index, src.ref)

            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, LocationRef):
                if src.type == dest.type:
                    pass
                elif isinstance(src.type, RoutineType) and isinstance(dest.type, VectorType):
                    self.assert_affected_within('inputs', src.type, dest.type.of_type)
                    self.assert_affected_within('outputs', src.type, dest.type.of_type)
                    self.assert_affected_within('trashes', src.type, dest.type.of_type)
                else:
                    raise TypeMismatchError(instr, (src, dest))
            else:
                raise TypeMismatchError(instr, (src, dest))

            # 2. check that the context is meaningful

            if isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndirectRef):
                context.assert_meaningful(src, REG_Y)
                # TODO this will need to be more sophisticated.  it's the thing ref points to that is written, not ref itself.
                context.set_written(dest.ref)
            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                context.assert_meaningful(src.ref, REG_Y)
                # TODO more sophisticated?
                context.set_written(dest)
            elif isinstance(src, IndirectRef) and isinstance(dest, IndirectRef):
                context.assert_meaningful(src.ref, REG_Y)
                # TODO more sophisticated?
                context.set_written(dest.ref)
            elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef):
                context.assert_meaningful(src, dest.ref, dest.index)
                context.set_written(dest.ref)
            elif isinstance(src, ConstantRef) and isinstance(dest, IndexedRef):
                context.assert_meaningful(src, dest.ref, dest.index)
                context.set_written(dest.ref)
            elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef):
                context.assert_meaningful(src.ref, src.index)
                context.set_touched(dest)
                context.set_written(dest)
            else:
                context.assert_meaningful(src)
                context.set_written(dest)

            context.set_touched(REG_A, FLAG_Z, FLAG_N)
            context.set_unmeaningful(REG_A, FLAG_Z, FLAG_N)
        elif opcode == 'goto':
            location = instr.location
            type_ = location.type
    
            if not isinstance(type_, (RoutineType, VectorType)):
                raise TypeMismatchError(instr, location)
    
            # assert that the dest routine's inputs are all initialized
            if isinstance(type_, VectorType):
                type_ = type_.of_type
            for ref in type_.inputs:
                context.assert_meaningful(ref)
    
            # and that this routine's trashes and output constraints are a
            # superset of the called routine's
            current_type = self.current_routine.location.type
            self.assert_affected_within('outputs', type_, current_type)
            self.assert_affected_within('trashes', type_, current_type)

            context.encounter_gotos(set([instr.location]))

            # now that we have encountered a goto here, we set the
            # context here to match what someone calling the goto'ed
            # function directly, would expect.  (which makes sense
            # when you think about it; if this goto's F, then calling
            # this is like calling F, from the perspective of what is
            # returned.

            for ref in type_.outputs:
                context.set_touched(ref)   # ?
                context.set_written(ref)

            for ref in type_.trashes:
                context.assert_writeable(ref)
                context.set_touched(ref)
                context.set_unmeaningful(ref)

            # TODO is that... all we have to do?  You'll note the above
            # is a lot like call.  We do rely on, if we are in a branch,
            # the branch-merge to take care of... a lot?  The fact that
            # we don't actually continue on from here, I mean.

        elif opcode == 'trash':
            context.set_touched(instr.dest)
            context.set_unmeaningful(instr.dest)
        elif opcode == 'nop':
            pass
        else:
            raise NotImplementedError(opcode)

    def analyze_if(self, instr, context):
        incoming_meaningful = set(context.each_meaningful())

        context1 = context.clone()
        context2 = context.clone()
        self.analyze_block(instr.block1, context1)
        if instr.block2 is not None:
            self.analyze_block(instr.block2, context2)

        outgoing_meaningful = set(context1.each_meaningful()) & set(context2.each_meaningful())
        outgoing_trashes = incoming_meaningful - outgoing_meaningful

        # TODO may we need to deal with touched separately here too?
        # probably not; if it wasn't meaningful in the first place, it
        # doesn't really matter if you modified it or not, coming out.
        for ref in context1.each_meaningful():
            if ref in outgoing_trashes:
                continue
            context2.assert_meaningful(
                ref, exception_class=InconsistentInitializationError,
                message='initialized in block 1 but not in block 2 of `if {}`'.format(instr.src)
            )
        for ref in context2.each_meaningful():
            if ref in outgoing_trashes:
                continue
            context1.assert_meaningful(
                ref, exception_class=InconsistentInitializationError,
                message='initialized in block 2 but not in block 1 of `if {}`'.format(instr.src)
            )

        # merge the contexts.  this used to be a method called `set_from`
        context._touched = set(context1._touched) | set(context2._touched)
        context.set_meaningful(*list(outgoing_meaningful))
        context._writeable = set(context1._writeable) | set(context2._writeable)
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

        # now analyze it having been executed a second time, with the context
        # of it having already been executed.
        self.analyze_block(instr.block, context)
        if instr.src is not None:
            context.assert_meaningful(instr.src)

        if context.encountered_gotos():
            raise IllegalJumpError(instr, instr)

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
