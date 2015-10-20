# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    TYPE_BYTE, TYPE_BYTE_TABLE,
    VectorType, ExecutableType,
    ConstantRef, LocationRef,
    REG_A, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)


class StaticAnalysisError(ValueError):
    pass


class UninitializedAccessError(StaticAnalysisError):
    pass


class UninitializedOutputError(StaticAnalysisError):
    pass


class InconsistentInitializationError(StaticAnalysisError):
    pass


class IllegalWriteError(StaticAnalysisError):
    pass


class UsageClashError(StaticAnalysisError):
    pass


class TypeMismatchError(StaticAnalysisError):
    pass


class IncompatibleConstraintsError(StaticAnalysisError):
    pass


class Context():
    """
    A location is touched if it was changed (or even potentially
    changed) during this routine, or some routine called by this routine.
    
    A location is meaningful if it was an input to this routine,
    or if it was set to a meaningful value by some operation in this
    routine (or some routine called by this routine.
    
    A location is writeable if it was listed in the outputs and trashes
    lists of this routine.
    """
    def __init__(self, inputs, outputs, trashes):
        self._touched = set()
        self._meaningful = set()
        self._writeable = set()

        for ref in inputs:
            self._meaningful.add(ref)
        output_names = set()
        for ref in outputs:
            output_names.add(ref.name)
            self._writeable.add(ref)
        for ref in trashes:
            if ref.name in output_names:
                raise UsageClashError(ref.name)
            self._writeable.add(ref)

    def clone(self):
        c = Context([], [], [])
        c._touched = set(self._touched)
        c._meaningful = set(self._meaningful)
        c._writeable = set(self._writeable)
        return c

    def set_from(self, c):
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
        exception_class = kwargs.get('exception_class', UninitializedAccessError)
        for ref in refs:
            if isinstance(ref, ConstantRef):
                pass
            elif isinstance(ref, LocationRef):
                if ref not in self._meaningful:
                    raise exception_class(ref.name)
            else:
                raise ValueError(ref)

    def assert_writeable(self, *refs):
        for ref in refs:
            if ref not in self._writeable:
                raise IllegalWriteError(ref.name)

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

def analyze_program(program):
    assert isinstance(program, Program)
    routines = {r.name: r for r in program.routines}
    for routine in program.routines:
        analyze_routine(routine, routines)


def analyze_routine(routine, routines):
    assert isinstance(routine, Routine)
    if routine.block is None:
        # it's an extern, that's fine
        return
    type = routine.location.type
    context = Context(type.inputs, type.outputs, type.trashes)
    analyze_block(routine.block, context, routines)
    for ref in type.outputs:
        context.assert_meaningful(ref, exception_class=UninitializedOutputError)
    for ref in context.each_touched():
        if ref not in type.outputs and ref not in type.trashes:
            raise IllegalWriteError(ref.name)


def analyze_block(block, context, routines):
    assert isinstance(block, Block)
    for i in block.instrs:
        analyze_instr(i, context, routines)


def analyze_instr(instr, context, routines):
    assert isinstance(instr, Instr)
    opcode = instr.opcode
    dest = instr.dest
    src = instr.src

    if opcode == 'ld':
        if instr.index:
            if src.type == TYPE_BYTE_TABLE and dest.type == TYPE_BYTE:
                pass
            else:
                raise TypeMismatchError((src, dest))
        elif src.type != dest.type:
            raise TypeMismatchError((src, dest))
        context.assert_meaningful(src)
        context.set_written(dest, FLAG_Z, FLAG_N)
    elif opcode == 'st':
        if instr.index:
            if src.type == TYPE_BYTE and dest.type == TYPE_BYTE_TABLE:
                pass
            else:
                raise TypeMismatchError((src, dest))
        elif src.type != dest.type:
            raise TypeMismatchError((src, dest))
        context.assert_meaningful(src)
        context.set_written(dest)
    elif opcode in ('add', 'sub'):
        context.assert_meaningful(src, dest, FLAG_C)
        context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
    elif opcode in ('inc', 'dec'):
        context.assert_meaningful(dest)
        context.set_written(dest, FLAG_Z, FLAG_N)
    elif opcode == 'cmp':
        context.assert_meaningful(src, dest)
        context.set_written(FLAG_Z, FLAG_N, FLAG_C)
    elif opcode in ('and', 'or', 'xor'):
        context.assert_meaningful(src, dest)
        context.set_written(dest, FLAG_Z, FLAG_N)
    elif opcode in ('shl', 'shr'):
        context.assert_meaningful(dest, FLAG_C)
        context.set_written(dest, FLAG_Z, FLAG_N, FLAG_C)
    elif opcode == 'call':
        routine = routines[instr.name]
        type = routine.location.type
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
        analyze_block(instr.block1, context1, routines)
        if instr.block2 is not None:
            analyze_block(instr.block2, context2, routines)
        # TODO may we need to deal with touched separately here too?
        # probably not; if it wasn't meaningful in the first place, it
        # doesn't really matter if you modified it or not, coming out.
        for ref in context1.each_meaningful():
            context2.assert_meaningful(ref, exception_class=InconsistentInitializationError)
        for ref in context2.each_meaningful():
            context1.assert_meaningful(ref, exception_class=InconsistentInitializationError)
        context.set_from(context1)
    elif opcode == 'repeat':
        # it will always be executed at least once, so analyze it having
        # been executed the first time.
        analyze_block(instr.block, context, routines)

        # now analyze it having been executed a second time, with the context
        # of it having already been executed.
        analyze_block(instr.block, context, routines)

        # NB I *think* that's enough... but it might not be?
    elif opcode == 'copy':
        # check that their types are basically compatible
        if src.type == dest.type:
            pass
        elif isinstance(src.type, ExecutableType) and \
             isinstance(dest.type, VectorType):
            pass
        else:
            raise TypeMismatchError((src, dest))

        # if dealing with routines and vectors,
        # check that they're not incompatible
        if isinstance(src.type, ExecutableType) and \
           isinstance(dest.type, VectorType):
            if not (src.type.inputs <= dest.type.inputs):
                raise IncompatibleConstraintsError(src.type.inputs - dest.type.inputs)
            if not (src.type.outputs <= dest.type.outputs):
                raise IncompatibleConstraintsError(src.type.outputs - dest.type.outputs)
            if not (src.type.trashes <= dest.type.trashes):
                raise IncompatibleConstraintsError(src.type.trashes - dest.type.trashes)
                
        context.assert_meaningful(src)
        context.set_written(dest)
        context.set_touched(REG_A, FLAG_Z, FLAG_N)
        context.set_unmeaningful(REG_A, FLAG_Z, FLAG_N)
    elif opcode == 'with-sei':
        analyze_block(instr.block, context, routines)
    else:
        raise NotImplementedError(opcode)
