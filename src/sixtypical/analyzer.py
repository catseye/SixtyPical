# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    TYPE_BYTE, TYPE_BYTE_TABLE, BufferType, PointerType, VectorType, ExecutableType,
    ConstantRef, LocationRef, IndirectRef, AddressRef,
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


class ConstraintsError(StaticAnalysisError):
    pass


class ConstantConstraintError(ConstraintsError):
    pass


class InconsistentConstraintsError(ConstraintsError):
    pass


class IncompatibleConstraintsError(ConstraintsError):
    pass


class Context(object):
    """
    A location is touched if it was changed (or even potentially
    changed) during this routine, or some routine called by this routine.
    
    A location is meaningful if it was an input to this routine,
    or if it was set to a meaningful value by some operation in this
    routine (or some routine called by this routine.
    
    A location is writeable if it was listed in the outputs and trashes
    lists of this routine.
    """
    def __init__(self, routines, routine, inputs, outputs, trashes):
        self.routines = routines    # Location -> AST node
        self.routine = routine
        self._touched = set()
        self._meaningful = set()
        self._writeable = set()

        for ref in inputs:
            if ref.is_constant():
                raise ConstantConstraintError('%s in %s' % (ref.name, routine.name))
            self._meaningful.add(ref)
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

    def clone(self):
        c = Context(self.routines, self.routine, [], [], [])
        c._touched = set(self._touched)
        c._meaningful = set(self._meaningful)
        c._writeable = set(self._writeable)
        return c

    def set_from(self, c):
        assert c.routines == self.routines
        assert c.routine == self.routine
        self._touched = set(c._touched)
        self._meaningful = set(c._meaningful)
        self._writeable = set(c._writeable)

    def each_meaningful(self):
        for ref in self._meaningful:
            yield ref

    def each_touched(self):
        for ref in self._touched:
            yield ref

    def assert_meaningful(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', UnmeaningfulReadError)
        for ref in refs:
            if ref.is_constant() or ref in self.routines:
                pass
            elif isinstance(ref, LocationRef):
                if ref not in self._meaningful:
                    message = '%s in %s' % (ref.name, self.routine.name)
                    if kwargs.get('message'):
                        message += ' (%s)' % kwargs['message']
                    raise exception_class(message)
            else:
                raise NotImplementedError(ref)

    def assert_writeable(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', ForbiddenWriteError)
        for ref in refs:
            if ref not in self._writeable:
                message = '%s in %s' % (ref.name, self.routine.name)
                if kwargs.get('message'):
                    message += ' (%s)' % kwargs['message']
                raise exception_class(message)

    def set_touched(self, *refs):
        for ref in refs:
            self._touched.add(ref)

    def set_meaningful(self, *refs):
        for ref in refs:
            self._meaningful.add(ref)

    def set_unmeaningful(self, *refs):
        for ref in refs:
            if ref in self._meaningful:
                self._meaningful.remove(ref)

    def set_written(self, *refs):
        """A "helper" method which does the following common sequence for
        the given refs: asserts they're all writable, and sets them all
        as touched and meaningful."""
        self.assert_writeable(*refs)
        self.set_touched(*refs)
        self.set_meaningful(*refs)


class Analyzer(object):

    def __init__(self):
        self.current_routine = None
        self.has_encountered_goto = False
        self.routines = {}

    def assert_type(self, type, *locations):
        for location in locations:
            if location.type != type:
                raise TypeMismatchError('%s in %s' %
                    (location.name, self.current_routine.name)
                )

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
        type = routine.location.type
        context = Context(self.routines, routine, type.inputs, type.outputs, type.trashes)
        self.analyze_block(routine.block, context)
        if not self.has_encountered_goto:
            for ref in type.outputs:
                context.assert_meaningful(ref, exception_class=UnmeaningfulOutputError)
            for ref in context.each_touched():
                if ref not in type.outputs and ref not in type.trashes:
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
            if instr.index:
                if src.type == TYPE_BYTE_TABLE and dest.type == TYPE_BYTE:
                    pass
                else:
                    raise TypeMismatchError('%s and %s in %s' %
                        (src.name, dest.name, self.current_routine.name)
                    )
            elif src.type != dest.type:
                raise TypeMismatchError('%s and %s in %s' %
                    (src.name, dest.name, self.current_routine.name)
                )
            context.assert_meaningful(src)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode == 'st':
            if instr.index:
                if src.type == TYPE_BYTE and dest.type == TYPE_BYTE_TABLE:
                    pass
                else:
                    raise TypeMismatchError((src, dest))
            elif src.type != dest.type:
                raise TypeMismatchError('%s and %s in %s' %
                    (src.name, dest.name, self.current_routine.name)
                )
            context.assert_meaningful(src)
            context.set_written(dest)
        elif opcode in ('add', 'sub'):
            self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest, FLAG_C)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
        elif opcode in ('inc', 'dec'):
            self.assert_type(TYPE_BYTE, dest)
            context.assert_meaningful(dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode == 'cmp':
            self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(FLAG_Z, FLAG_N, FLAG_C)
        elif opcode in ('and', 'or', 'xor'):
            self.assert_type(TYPE_BYTE, src, dest)
            context.assert_meaningful(src, dest)
            context.set_written(dest, FLAG_Z, FLAG_N)
        elif opcode in ('shl', 'shr'):
            self.assert_type(TYPE_BYTE, dest)
            context.assert_meaningful(dest, FLAG_C)
            context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C)
        elif opcode == 'call':
            type = instr.location.type
            for ref in type.inputs:
                context.assert_meaningful(ref)
            for ref in type.outputs:
                context.set_written(ref)
            for ref in type.trashes:
                context.assert_writeable(ref)
                context.set_touched(ref)
                context.set_unmeaningful(ref)
        elif opcode == 'if':
            context1 = context.clone()
            context2 = context.clone()
            self.analyze_block(instr.block1, context1)
            if instr.block2 is not None:
                self.analyze_block(instr.block2, context2)
            # TODO may we need to deal with touched separately here too?
            # probably not; if it wasn't meaningful in the first place, it
            # doesn't really matter if you modified it or not, coming out.
            for ref in context1.each_meaningful():
                context2.assert_meaningful(
                    ref, exception_class=InconsistentInitializationError, message='initialized in block 1 but not in block 2'
                )
            for ref in context2.each_meaningful():
                context1.assert_meaningful(
                    ref, exception_class=InconsistentInitializationError, message='initialized in block 2 but not in block 1'
                )
            context.set_from(context1)
        elif opcode == 'repeat':
            # it will always be executed at least once, so analyze it having
            # been executed the first time.
            self.analyze_block(instr.block, context)
            context.assert_meaningful(src)

            # now analyze it having been executed a second time, with the context
            # of it having already been executed.
            self.analyze_block(instr.block, context)
            context.assert_meaningful(src)

        elif opcode == 'copy':
            # 1. check that their types are compatible

            if isinstance(dest, IndirectRef):
                if src.type == TYPE_BYTE and isinstance(dest.ref.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError((src, dest))
            elif isinstance(src, AddressRef):
                if isinstance(src.ref.type, BufferType) and isinstance(dest.type, PointerType):
                    pass
                else:
                    raise TypeMismatchError((src, dest))

            elif isinstance(src, (LocationRef, ConstantRef)) and isinstance(dest, LocationRef):
                if src.type == dest.type:
                    pass
                elif isinstance(src.type, ExecutableType) and \
                     isinstance(dest.type, VectorType):
                    # if dealing with routines and vectors,
                    # check that they're not incompatible
                    if not (src.type.inputs <= dest.type.inputs):
                        raise IncompatibleConstraintsError(src.type.inputs - dest.type.inputs)
                    if not (src.type.outputs <= dest.type.outputs):
                        raise IncompatibleConstraintsError(src.type.outputs - dest.type.outputs)
                    if not (src.type.trashes <= dest.type.trashes):
                        raise IncompatibleConstraintsError(src.type.trashes - dest.type.trashes)
                else:
                    raise TypeMismatchError((src, dest))
            else:
                raise TypeMismatchError((src, dest))

            # 2. check that the context is meaningful

            if isinstance(dest, IndirectRef):
                context.assert_meaningful(src, REG_Y)
                # TODO this will need to be more sophisticated.  it's the thing ref points to that is written, not ref itself.
                context.set_written(dest.ref)
            else:
                context.assert_meaningful(src)
                context.set_written(dest)

            context.set_touched(REG_A, FLAG_Z, FLAG_N)
            context.set_unmeaningful(REG_A, FLAG_Z, FLAG_N)

        elif opcode == 'with-sei':
            self.analyze_block(instr.block, context)
        elif opcode == 'goto':
            location = instr.location
            type = location.type
    
            if not isinstance(type, ExecutableType):
                raise TypeMismatchError(location)
    
            # assert that the dest routine's inputs are all initialized
            for ref in type.inputs:
                context.assert_meaningful(ref)
    
            # and that this routine's trashes and output constraints are a
            # superset of the called routine's
            current_type = self.current_routine.location.type
            if not (type.outputs <= current_type.outputs):
                raise IncompatibleConstraintsError(type.outputs - current_type.outputs)
            if not (type.trashes <= current_type.trashes):
                raise IncompatibleConstraintsError(type.trashes - current_type.trashes)
            self.has_encountered_goto = True
        else:
            raise NotImplementedError(opcode)
