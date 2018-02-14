# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    TYPE_BYTE, TYPE_WORD,
    TableType, BufferType, PointerType, VectorType, RoutineType,
    ConstantRef, LocationRef, IndirectRef, IndexedRef, AddressRef,
    REG_A, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)


class StaticAnalysisError(ValueError):
    pass


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
    lists of this routine.
    """
    def __init__(self, routines, routine, inputs, outputs, trashes):
        self.routines = routines    # Location -> AST node
        self.routine = routine
        self._touched = set()
        self._range = dict()
        self._writeable = set()

        for ref in inputs:
            if ref.is_constant():
                raise ConstantConstraintError('%s in %s' % (ref.name, routine.name))
            self._range[ref] = ref.max_range()
        output_names = set()
        for ref in outputs:
            if ref.is_constant():
                raise ConstantConstraintError('%s in %s' % (ref.name, routine.name))
            output_names.add(ref.name)
            self._writeable.add(ref)
        for ref in trashes:
            if ref.is_constant():
                raise ConstantConstraintError('%s in %s' % (ref.name, routine.name))
            if ref.name in output_names:
                raise InconsistentConstraintsError('%s in %s' % (ref.name, routine.name))
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
                    message = '%s in %s' % (ref.name, self.routine.name)
                    if kwargs.get('message'):
                        message += ' (%s)' % kwargs['message']
                    raise exception_class(message)
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
                message = '%s in %s' % (ref.name, self.routine.name)
                if kwargs.get('message'):
                    message += ' (%s)' % kwargs['message']
                raise exception_class(message)

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
            raise RangeExceededError(
                "Possible range of {} {} exceeds acceptable range of {} {}".format(
                    inside, inside_range, outside, outside_range
                )
            )

    def set_touched(self, *refs):
        for ref in refs:
            self._touched.add(ref)

    def set_meaningful(self, *refs):
        for ref in refs:
            if ref not in self._range:
                self._range[ref] = ref.max_range()

    def set_top_of_range(self, ref, top):
        self.assert_meaningful(ref)
        (bottom, _) = self._range[ref]
        self._range[ref] = (bottom, top)

    def get_top_of_range(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        self.assert_meaningful(ref)
        (_, top) = self._range[ref]
        return top

    def copy_range(self, src, dest):
        self.assert_meaningful(src)
        if src in self._range:
            src_range = self._range[src]
        else:
            src_range = src.max_range()
        self._range[dest] = src_range

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


class Analyzer(object):

    def __init__(self, debug=False):
        self.current_routine = None
        self.has_encountered_goto = False
        self.routines = {}
        self.debug = debug

    def assert_type(self, type, *locations):
        for location in locations:
            if location.type != type:
                raise TypeMismatchError('%s in %s' %
                    (location.name, self.current_routine.name)
                )

    def assert_affected_within(self, name, affecting_type, limiting_type):
        assert name in ('inputs', 'outputs', 'trashes')
        affected = getattr(affecting_type, name)
        limited_to = getattr(limiting_type, name)
        overage = affected - limited_to
        if not overage:
            return
        message = 'in %s: %s for %s are %s\n\nbut %s affects %s\n\nwhich exceeds it by: %s ' % (
            self.current_routine.name, name,
            limiting_type, LocationRef.format_set(limited_to),
            affecting_type, LocationRef.format_set(affected),
            LocationRef.format_set(overage)
        )
        raise IncompatibleConstraintsError(message)

    def analyze_program(self, program):
        assert isinstance(program, Program)
        self.routines = {r.location: r for r in program.routines}
        for routine in program.routines:
            self.analyze_routine(routine)

    def analyze_routine(self, routine):
        assert isinstance(routine, Routine)
        self.current_routine = routine
        self.has_encountered_goto = False
        if routine.block is None:
            # it's an extern, that's fine
            return
        type_ = routine.location.type
        context = Context(self.routines, routine, type_.inputs, type_.outputs, type_.trashes)

        if self.debug:
            print "at start of routine `{}`:".format(routine.name)
            print context

        self.analyze_block(routine.block, context)
        trashed = set(context.each_touched()) - set(context.each_meaningful())

        if self.debug:
            print "at end of routine `{}`:".format(routine.name)
            print context
            print "trashed: ", LocationRef.format_set(trashed)
            print "outputs: ", LocationRef.format_set(type_.outputs)
            trashed_outputs = type_.outputs & trashed
            if trashed_outputs:
                print "TRASHED OUTPUTS: ", LocationRef.format_set(trashed_outputs)
            print ''
            print '-' * 79
            print ''

        # even if we goto another routine, we can't trash an output.
        for ref in trashed:
            if ref in type_.outputs:
                raise UnmeaningfulOutputError('%s in %s' % (ref.name, routine.name))

        if not self.has_encountered_goto:
            for ref in type_.outputs:
                context.assert_meaningful(ref, exception_class=UnmeaningfulOutputError)
            for ref in context.each_touched():
                if ref not in type_.outputs and ref not in type_.trashes and not routine_has_static(routine, ref):
                    message = '%s in %s' % (ref.name, routine.name)
                    raise ForbiddenWriteError(message)
        self.current_routine = None

    def analyze_block(self, block, context):
        assert isinstance(block, Block)
        for i in block.instrs:
            if self.has_encountered_goto:
                raise IllegalJumpError(i)
            self.analyze_instr(i, context)

    def analyze_instr(self, instr, context):
        assert isinstance(instr, Instr)
        opcode = instr.opcode
        dest = instr.dest
        src = instr.src
    
        if opcode == 'ld':
            if isinstance(src, IndexedRef):
                if TableType.is_a_table_type(src.ref.type, TYPE_BYTE) and dest.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError('%s and %s in %s' %
                        (src.ref.name, dest.name, self.current_routine.name)
                    )
                context.assert_meaningful(src, src.index)
                context.assert_in_range(src.index, src.ref)
            elif isinstance(src, IndirectRef):
                # copying this analysis from the matching branch in `copy`, below
                if isinstance(src.ref.type, PointerType) and dest.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError((src, dest))
                context.assert_meaningful(src.ref, REG_Y)
            elif src.type != dest.type:
                raise TypeMismatchError('%s and %s in %s' %
                    (src.name, dest.name, self.current_routine.name)
                )
            else:
                context.assert_meaningful(src)
                context.copy_range(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode == 'st':
            if isinstance(dest, IndexedRef):
                if src.type == TYPE_BYTE and TableType.is_a_table_type(dest.ref.type, TYPE_BYTE):
                    pass
                else:
                    raise TypeMismatchError((src, dest))
                context.assert_meaningful(dest.index)
                context.assert_in_range(dest.index, dest.ref)
                context.set_written(dest.ref)
            elif isinstance(dest, IndirectRef):
                # copying this analysis from the matching branch in `copy`, below
                if isinstance(dest.ref.type, PointerType) and src.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError((src, dest))
                context.assert_meaningful(dest.ref, REG_Y)
                context.set_written(dest.ref)
            elif src.type != dest.type:
                raise TypeMismatchError('%r and %r in %s' %
                    (src, dest, self.current_routine.name)
                )
            else:
                context.set_written(dest)
            context.assert_meaningful(src)
        elif opcode == 'add':
            context.assert_meaningful(src, dest, FLAG_C)
            if src.type == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
                context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
            else:
                self.assert_type(TYPE_WORD, src)
                if dest.type == TYPE_WORD:
                    context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
                elif isinstance(dest.type, PointerType):
                    context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
                    context.set_touched(REG_A)
                    context.set_unmeaningful(REG_A)
                else:
                    self.assert_type(TYPE_WORD, dest)
        elif opcode == 'sub':
            context.assert_meaningful(src, dest, FLAG_C)
            if src.type == TYPE_BYTE:
                self.assert_type(TYPE_BYTE, src, dest)
                context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
            else:
                self.assert_type(TYPE_WORD, src, dest)
                context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
                context.set_touched(REG_A)
                context.set_unmeaningful(REG_A)
        elif opcode in ('inc', 'dec'):
            self.assert_type(TYPE_BYTE, dest)
            context.assert_meaningful(dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode == 'cmp':
            self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(FLAG_Z, FLAG_N, FLAG_C)
        elif opcode == 'and':
            self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
            # If you AND the A register with a value V, the resulting value of A
            # cannot exceed the value of V; i.e. the maximum value of A becomes
            # the maximum value of V.
            context.set_top_of_range(dest, context.get_top_of_range(src))
        elif opcode in ('or', 'xor'):
            self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode in ('shl', 'shr'):
            self.assert_type(TYPE_BYTE, dest)
            context.assert_meaningful(dest, FLAG_C)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C)
        elif opcode == 'call':
            type = instr.location.type
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
        elif opcode == 'if':
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
                    message='initialized in block 1 but not in block 2 of `if {}`'.format(src)
                )
            for ref in context2.each_meaningful():
                if ref in outgoing_trashes:
                    continue
                context1.assert_meaningful(
                    ref, exception_class=InconsistentInitializationError,
                    message='initialized in block 2 but not in block 1 of `if {}`'.format(src)
                )

            # merge the contexts.  this used to be a method called `set_from`
            context._touched = set(context1._touched) | set(context2._touched)
            context.set_meaningful(*list(outgoing_meaningful))
            context._writeable = set(context1._writeable) | set(context2._writeable)

            for ref in outgoing_trashes:
                context.set_touched(ref)
                context.set_unmeaningful(ref)

        elif opcode == 'repeat':
            # it will always be executed at least once, so analyze it having
            # been executed the first time.
            self.analyze_block(instr.block, context)
            if src is not None:  # None indicates 'repeat forever'
                context.assert_meaningful(src)

            # now analyze it having been executed a second time, with the context
            # of it having already been executed.
            self.analyze_block(instr.block, context)
            if src is not None:
                context.assert_meaningful(src)

        elif opcode == 'copy':
            if dest == REG_A:
                raise ForbiddenWriteError("{} cannot be used as destination for copy".format(dest))

            # 1. check that their types are compatible

            if isinstance(src, AddressRef) and isinstance(dest, LocationRef):
                if isinstance(src.ref.type, BufferType) and isinstance(dest.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError((src, dest))
            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndirectRef):
                if src.type == TYPE_BYTE and isinstance(dest.ref.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError((src, dest))
            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                if isinstance(src.ref.type, PointerType) and dest.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError((src, dest))

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
                    raise TypeMismatchError((src, dest))

            elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef):
                if TableType.is_a_table_type(src.ref.type, TYPE_WORD) and dest.type == TYPE_WORD:
                    pass
                elif (isinstance(src.ref.type, TableType) and isinstance(dest.type, VectorType) and
                      RoutineType.executable_types_compatible(src.ref.type.of_type, dest.type.of_type)):
                    pass
                else:
                    raise TypeMismatchError((src, dest))

            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, LocationRef):
                if src.type == dest.type:
                    pass
                elif isinstance(src.type, RoutineType) and isinstance(dest.type, VectorType):
                    self.assert_affected_within('inputs', src.type, dest.type.of_type)
                    self.assert_affected_within('outputs', src.type, dest.type.of_type)
                    self.assert_affected_within('trashes', src.type, dest.type.of_type)
                else:
                    raise TypeMismatchError((src, dest))
            else:
                raise TypeMismatchError((src, dest))

            # 2. check that the context is meaningful

            if isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, IndirectRef):
                context.assert_meaningful(src, REG_Y)
                # TODO this will need to be more sophisticated.  it's the thing ref points to that is written, not ref itself.
                context.set_written(dest.ref)
            elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef):
                context.assert_meaningful(src.ref, REG_Y)
                context.set_written(dest)
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

        elif opcode == 'with-sei':
            self.analyze_block(instr.block, context)
        elif opcode == 'goto':
            location = instr.location
            type_ = location.type
    
            if not isinstance(type_, (RoutineType, VectorType)):
                raise TypeMismatchError(location)
    
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

            self.has_encountered_goto = True
        elif opcode == 'trash':
            context.set_touched(instr.dest)
            context.set_unmeaningful(instr.dest)
        else:
            raise NotImplementedError(opcode)
