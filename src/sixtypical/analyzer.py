# encoding: UTF-8

from sixtypical.ast import (
    Program, Routine, Block, SingleOp, Call, GoTo, If, Repeat, For, WithInterruptsOff, Save, PointInto
)
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


class AnalysisContext(object):
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
    def __init__(self, symtab, routine, inputs, outputs, trashes):
        self.symtab = symtab
        self.routine = routine           # Routine (AST node)
        self._touched = set()            # {LocationRef}
        self._range = dict()             # LocationRef -> (Int, Int)
        self._writeable = set()          # {LocationRef}
        self._terminated = False
        self._gotos_encountered = set()
        self._pointer_assoc = dict()

        for ref in inputs:
            if self.is_constant(ref):
                raise ConstantConstraintError(self.routine, ref.name)
            self._range[ref] = self.max_range(ref)
        output_names = set()
        for ref in outputs:
            if self.is_constant(ref):
                raise ConstantConstraintError(self.routine, ref.name)
            output_names.add(ref.name)
            self._writeable.add(ref)
        for ref in trashes:
            if self.is_constant(ref):
                raise ConstantConstraintError(self.routine, ref.name)
            if ref.name in output_names:
                raise InconsistentConstraintsError(self.routine, ref.name)
            self._writeable.add(ref)

    def __str__(self):
        return "{}(\n  _touched={},\n  _range={},\n  _writeable={}\n)".format(
            self.__class__.__name__,
            LocationRef.format_set(self._touched), LocationRef.format_set(self._range), LocationRef.format_set(self._writeable)
        )

    def to_json_data(self):
        type_ = self.symtab.fetch_global_type(self.routine.name)
        return {
            'routine_inputs': ','.join(sorted(loc.name for loc in type_.inputs)),
            'routine_outputs': ','.join(sorted(loc.name for loc in type_.outputs)),
            'routine_trashes': ','.join(sorted(loc.name for loc in type_.trashes)),
            'touched': ','.join(sorted(loc.name for loc in self._touched)),
            'range': dict((loc.name, '{}-{}'.format(rng[0], rng[1])) for (loc, rng) in self._range.items()),
            'writeable': ','.join(sorted(loc.name for loc in self._writeable)),
            'terminated': self._terminated,
            'gotos_encountered': ','.join(sorted(loc.name for loc in self._gotos_encountered)),
        }

    def clone(self):
        c = AnalysisContext(self.symtab, self.routine, [], [], [])
        c._touched = set(self._touched)
        c._range = dict(self._range)
        c._writeable = set(self._writeable)
        c._pointer_assoc = dict(self._pointer_assoc)
        c._gotos_encountered = set(self._gotos_encountered)
        return c

    def update_from(self, other):
        """Replaces the information in this context, with the information from the other context.
        This is an overwriting action - it does not attempt to merge the contexts.

        We do not replace the gotos_encountered for technical reasons.  (In `analyze_if`,
        we merge those sets afterwards; at the end of `analyze_routine`, they are not distinct in the
        set of contexts we are updating from, and we want to retain our own.)"""
        self.routine = other.routine
        self._touched = set(other._touched)
        self._range = dict(other._range)
        self._writeable = set(other._writeable)
        self._terminated = other._terminated
        self._pointer_assoc = dict(other._pointer_assoc)

    def each_meaningful(self):
        for ref in self._range.keys():
            yield ref

    def each_touched(self):
        for ref in self._touched:
            yield ref

    def each_writeable(self):
        for ref in self._writeable:
            yield ref

    def assert_meaningful(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', UnmeaningfulReadError)
        for ref in refs:
            # statics are always meaningful
            if self.symtab.has_static(self.routine.name, ref.name):
                continue
            if self.is_constant(ref):
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
            if self.symtab.has_static(self.routine.name, ref.name):
                continue
            if ref not in self._writeable:
                message = ref.name
                if kwargs.get('message'):
                    message += ' (%s)' % kwargs['message']
                raise exception_class(self.routine, message)

    def assert_in_range(self, inside, outside, offset):
        """Given two locations, assert that the first location, offset by the given offset,
        is contained 'inside' the second location."""
        assert isinstance(inside, LocationRef)
        assert isinstance(outside, LocationRef)

        # inside should always be meaningful
        inside_range = self._range[inside]

        # outside might not be meaningful, so default to max range if necessary
        if outside in self._range:
            outside_range = self._range[outside]
        else:
            outside_range = self.max_range(outside)

        if (inside_range[0] + offset.value) < outside_range[0] or (inside_range[1] + offset.value) > outside_range[1]:
            raise RangeExceededError(self.routine,
                "Possible range of {} {} (+{}) exceeds acceptable range of {} {}".format(
                    inside, inside_range, offset, outside, outside_range
                )
            )

    def set_touched(self, *refs):
        for ref in refs:
            self._touched.add(ref)
            # TODO: it might be possible to invalidate the range here

    def set_meaningful(self, *refs):
        for ref in refs:
            if ref not in self._range:
                self._range[ref] = self.max_range(ref)

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
            src_range = self.max_range(src)
        self._range[dest] = src_range

    def invalidate_range(self, ref):
        self.assert_meaningful(ref)
        self._range[ref] = self.max_range(ref)

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

    def set_terminated(self):
        # Having a terminated context and having encountered gotos is not the same thing.
        self._terminated = True

    def has_terminated(self):
        return self._terminated

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

    def get_assoc(self, pointer):
        return self._pointer_assoc.get(pointer)

    def set_assoc(self, pointer, table):
        self._pointer_assoc[pointer] = table

    def is_constant(self, ref):
        """read-only means that the program cannot change the value
        of a location.  constant means that the value of the location
        will not change during the lifetime of the program."""
        if isinstance(ref, ConstantRef):
            return True
        if isinstance(ref, (IndirectRef, IndexedRef)):
            return False
        if isinstance(ref, LocationRef):
            type_ = self.symtab.fetch_global_type(ref.name)
            return isinstance(type_, RoutineType)
        raise NotImplementedError

    def max_range(self, ref):
        if isinstance(ref, ConstantRef):
            return (ref.value, ref.value)
        elif self.symtab.has_static(self.routine.name, ref.name):
            return self.symtab.fetch_static_type(self.routine.name, ref.name).max_range
        else:
            return self.symtab.fetch_global_type(ref.name).max_range


class Analyzer(object):

    def __init__(self, symtab, debug=False):
        self.symtab = symtab
        self.current_routine = None
        self.debug = debug
        self.exit_contexts_map = {}

    # - - - - helper methods - - - -

    def get_type_for_name(self, name):
        if self.current_routine and self.symtab.has_static(self.current_routine.name, name):
            return self.symtab.fetch_static_type(self.current_routine.name, name)
        return self.symtab.fetch_global_type(name)

    def get_type(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.type
        if not isinstance(ref, LocationRef):
            raise NotImplementedError
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
            context = self.analyze_routine(routine)
            routine.encountered_gotos = list(context.encountered_gotos()) if context else []

    def analyze_routine(self, routine):
        assert isinstance(routine, Routine)
        if routine.block is None:
            # it's an extern, that's fine
            return None

        self.current_routine = routine
        type_ = self.get_type_for_name(routine.name)
        context = AnalysisContext(self.symtab, routine, type_.inputs, type_.outputs, type_.trashes)
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
            if ref not in type_.outputs and ref not in type_.trashes and not self.symtab.has_static(routine.name, ref.name):
                raise ForbiddenWriteError(routine, ref.name)

        self.exit_contexts = None
        self.current_routine = None
        return context

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
            self.analyze_block(instr.block, context)
            if context.encountered_gotos():
                raise IllegalJumpError(instr, instr)
        elif isinstance(instr, Save):
            self.analyze_save(instr, context)
        elif isinstance(instr, PointInto):
            self.analyze_point_into(instr, context)
        else:
            raise NotImplementedError

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
                context.set_touched(target)
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
                context.assert_meaningful(src, REG_Y)

                target = context.get_assoc(dest.ref)
                if not target:
                    raise ForbiddenWriteError(instr, dest.ref)
                context.set_touched(target)
                context.set_written(target)

            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                context.assert_meaningful(src.ref, REG_Y)

                origin = context.get_assoc(src.ref)
                if not origin:
                    raise UnmeaningfulReadError(instr, src.ref)
                context.assert_meaningful(origin)

                context.set_touched(dest)
                context.set_written(dest)
            elif isinstance(src, IndirectRef) and isinstance(dest, IndirectRef):
                context.assert_meaningful(src.ref, REG_Y)

                origin = context.get_assoc(src.ref)
                if not origin:
                    raise UnmeaningfulReadError(instr, src.ref)
                context.assert_meaningful(origin)

                target = context.get_assoc(dest.ref)
                if not target:
                    raise ForbiddenWriteError(instr, dest.ref)
                context.set_touched(target)
                context.set_written(target)

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

        elif opcode == 'trash':
            context.set_touched(instr.dest)
            context.set_unmeaningful(instr.dest)
        elif opcode == 'nop':
            pass
        else:
            raise NotImplementedError(opcode)

    def analyze_call(self, instr, context):
        type = self.get_type(instr.location)
        if not isinstance(type, (RoutineType, VectorType)):
            raise TypeMismatchError(instr, instr.location.name)
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

    def analyze_goto(self, instr, context):
        location = instr.location
        type_ = self.get_type(instr.location)

        if not isinstance(type_, (RoutineType, VectorType)):
            raise TypeMismatchError(instr, location.name)

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
            exit_context.set_touched(ref)   # ?
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

        # associate pointer with table, mark it as meaningful.

        context.set_assoc(instr.pointer, instr.table)
        context.set_meaningful(instr.pointer)
        context.set_touched(instr.pointer)

        self.analyze_block(instr.block, context)
        if context.encountered_gotos():
            raise IllegalJumpError(instr, instr)

        # unassociate pointer with table, mark as unmeaningful.

        context.set_assoc(instr.pointer, None)
        context.set_unmeaningful(instr.pointer)
