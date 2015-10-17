# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    ConstantRef, LocationRef, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)


UNINITIALIZED = 'UNINITIALIZED'
INITIALIZED = 'INITIALIZED'


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


class Context():
    def __init__(self, inputs, outputs, trashes):
        self._store = {}
        self._writeables = set()

        for ref in inputs:
            self._store.setdefault(ref.name, INITIALIZED)
        output_names = set()
        for ref in outputs:
            output_names.add(ref.name)
            self._store.setdefault(ref.name, UNINITIALIZED)
            self._writeables.add(ref.name)
        for ref in trashes:
            if ref.name in output_names:
                raise UsageClashError(ref.name)
            self._store.setdefault(ref.name, UNINITIALIZED)
            self._writeables.add(ref.name)

    def clone(self):
        c = Context([], [], [])
        c._store = dict(self._store)
        c._writeables = set(self._writeables)
        return c

    def set_from(self, c):
        self._store = dict(c._store)
        self._writeables = set(c._writeables)

    def each_initialized(self):
        for key, value in self._store.iteritems():
            if value == INITIALIZED:
                yield LocationRef(key)

    def assert_initialized(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', UninitializedAccessError)
        for ref in refs:
            if isinstance(ref, ConstantRef):
                pass
            elif isinstance(ref, LocationRef):
                if self.get(ref) != INITIALIZED:
                    raise exception_class(ref.name)
            else:
                raise ValueError(ref)

    def assert_writeable(self, *refs):
        for ref in refs:
            if ref.name not in self._writeables:
                raise IllegalWriteError(ref.name)

    def set_initialized(self, *refs):
        for ref in refs:
            self.set(ref, INITIALIZED)

    def set_uninitialized(self, *refs):
        for ref in refs:
            self.set(ref, UNINITIALIZED)

    def get(self, ref):
        if isinstance(ref, ConstantRef):
            return INITIALIZED
        elif isinstance(ref, LocationRef):
            if ref.name not in self._store:
                return UNINITIALIZED
            return self._store[ref.name]
        else:
            raise ValueError(ref)

    def set(self, ref, value):
        assert isinstance(ref, LocationRef)
        self._store[ref.name] = value


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
    context = Context(routine.inputs, routine.outputs, routine.trashes)
    analyze_block(routine.block, context, routines)
    for ref in routine.outputs:
        context.assert_initialized(ref, exception_class=UninitializedOutputError)


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
        context.assert_initialized(src)
        context.assert_writeable(dest, FLAG_Z, FLAG_N)
        context.set_initialized(dest, FLAG_Z, FLAG_N)
    elif opcode == 'st':
        context.assert_initialized(src)
        context.assert_writeable(dest)
        context.set_initialized(dest)
    elif opcode in ('add', 'sub'):
        context.assert_initialized(src, dest, FLAG_C)
        context.assert_writeable(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
        context.set_initialized(dest, FLAG_Z, FLAG_N, FLAG_C, FLAG_V)
    elif opcode in ('inc', 'dec'):
        context.assert_initialized(dest)
        context.assert_writeable(dest, FLAG_Z, FLAG_N)
        context.set_initialized(dest, FLAG_Z, FLAG_N)
    elif opcode == 'cmp':
        context.assert_initialized(src, dest)
        context.assert_writeable(FLAG_Z, FLAG_N, FLAG_C)
        context.set_initialized(FLAG_Z, FLAG_N, FLAG_C)
    elif opcode in ('and', 'or', 'xor'):
        context.assert_initialized(src, dest)
        context.assert_writeable(dest, FLAG_Z, FLAG_N)
        context.set_initialized(dest, FLAG_Z, FLAG_N)
    elif opcode in ('shl', 'shr'):
        context.assert_initialized(dest, FLAG_C)
        context.assert_writeable(dest, FLAG_Z, FLAG_N, FLAG_C)
        context.set_initialized(dest, FLAG_Z, FLAG_N, FLAG_C)
    elif opcode == 'call':
        routine = routines[instr.name]
        for ref in routine.inputs:
            context.assert_initialized(ref)
        for ref in routine.outputs:
            context.assert_writeable(ref)
            context.set_initialized(ref)
        for ref in routine.trashes:
            context.assert_writeable(ref)
            context.set_uninitialized(ref)
    elif opcode == 'if':
        context1 = context.clone()
        context2 = context.clone()
        analyze_block(instr.block1, context1, routines)
        if instr.block2 is not None:
            analyze_block(instr.block2, context2, routines)
        for ref in context1.each_initialized():
            context2.assert_initialized(ref, exception_class=InconsistentInitializationError)
        for ref in context2.each_initialized():
            context1.assert_initialized(ref, exception_class=InconsistentInitializationError)
        context.set_from(context1)
    else:
        raise NotImplementedError(opcode)
