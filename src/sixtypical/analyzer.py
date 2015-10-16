# encoding: UTF-8

import sys

from sixtypical.ast import Program, Defn, Routine, Block, Instr
from sixtypical.model import ConstantRef, LocationRef


UNINITIALIZED = 'UNINITIALIZED'
INITIALIZED = 'INITIALIZED'


class StaticAnalysisError(ValueError):
    pass


class UninitializedAccessError(StaticAnalysisError):
    pass


class UninitializedOutputError(StaticAnalysisError):
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

    def assertInitialized(self, *refs, **kwargs):
        exception_class = kwargs.get('exception_class', UninitializedAccessError)
        for ref in refs:
            if isinstance(ref, ConstantRef):
                pass
            elif isinstance(ref, LocationRef):
                if self.get(ref) != INITIALIZED:
                    raise exception_class(ref.name)
            else:
                raise ValueError(ref)

    def assertWriteable(self, *refs):
        for ref in refs:
            if ref.name not in self._writeables:
                raise IllegalWriteError(ref.name)

    def setInitialized(self, *refs):
        for ref in refs:
            self.set(ref, INITIALIZED)

    def setUninitialized(self, *refs):
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
    context = Context(routine.inputs, routine.outputs, routine.trashes)
    analyze_block(routine.block, context, routines)
    for ref in routine.outputs:
        context.assertInitialized(ref, exception_class=UninitializedOutputError)


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
        context.assertInitialized(src)
        context.assertWriteable(dest, LocationRef('z'), LocationRef('n'))
        context.setInitialized(dest, LocationRef('z'), LocationRef('n'))
    elif opcode == 'st':
        context.assertInitialized(src)
        context.assertWriteable(dest)
        context.setInitialized(dest)
    elif opcode in ('add', 'sub'):
        context.assertInitialized(src, dest, LocationRef('c'))
        context.assertWriteable(dest,
            LocationRef('z'), LocationRef('n'),
            LocationRef('c'), LocationRef('v'),
        )
        context.setInitialized(dest,
            LocationRef('z'), LocationRef('n'),
            LocationRef('c'), LocationRef('v'),
        )
    elif opcode in ('inc', 'dec'):
        context.assertInitialized(dest)
        context.assertWriteable(dest, LocationRef('z'), LocationRef('n'))
        context.setInitialized(dest, LocationRef('z'), LocationRef('n'))
    elif opcode == 'cmp':
        context.assertInitialized(src, dest)
        context.assertWriteable(LocationRef('z'), LocationRef('n'), LocationRef('c'))
        context.setInitialized(LocationRef('z'), LocationRef('n'), LocationRef('c'))
    elif opcode in ('and', 'or', 'xor'):
        context.assertInitialized(sec, dest)
        context.assertWriteable(dest, LocationRef('z'), LocationRef('n'))
        context.setInitialized(dest, LocationRef('z'), LocationRef('n'))
    elif opcode in ('shl', 'shr'):
        context.assertInitialized(dest)
        context.assertWriteable(dest, LocationRef('z'), LocationRef('n'), LocationRef('c'))
        context.setInitialized(dest, LocationRef('z'), LocationRef('n'), LocationRef('c'))
    elif opcode == 'call':
        routine = routines[instr.name]
        for ref in routine.inputs:
            context.assertInitialized(ref)
        for ref in routine.outputs:
            context.assertWriteable(ref)
            context.setInitialized(ref)
        for ref in routine.trashes:
            context.assertWriteable(ref)
            context.setUninitialized(ref)
    elif opcode == 'if':
        context1 = context.clone()
        context2 = context.clone()
        analyze_block(instr.block1, context1, routines)
        analyze_block(instr.block2, context2, routines)
        reconcile_contexts(context1, context2, output=context)
    else:
        raise NotImplementedError
