# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    ConstantRef, LocationRef,
    REG_A, REG_X, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)


class Context(object):
    def __init__(self):
        self._store = {}

    def __str__(self):
        return '\n'.join("%s: %s" % p for p in sorted(self._store.iteritems()))

    def get(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        elif isinstance(ref, LocationRef):
            return self._store[ref.name]
        else:
            raise ValueError(ref)

    def set(self, ref, value):
        assert isinstance(ref, LocationRef)
        self._store[ref.name] = value


def eval_program(program):
    assert isinstance(program, Program)
    routines = {r.name: r for r in program.routines}
    context = Context()
    for ref in (REG_A, REG_X, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C):
        context.set(ref, 0)
    eval_routine(routines['main'], context, routines)
    return context


def eval_routine(routine, context, routines):
    assert isinstance(routine, Routine)
    eval_block(routine.block, context, routines)


def eval_block(block, context, routines):
    assert isinstance(block, Block)
    for i in block.instrs:
        eval_instr(i, context, routines)


def eval_instr(instr, context, routines):
    assert isinstance(instr, Instr)
    opcode = instr.opcode
    dest = instr.dest
    src = instr.src

    if opcode == 'ld':
        result = context.get(src)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'st':
        context.set(dest, context.get(src))
    elif opcode == 'add':
        carry = context.get(FLAG_C)
        val = context.get(src)
        now = context.get(dest)
        result = now + val + carry
        if result > 255:
            result &= 255
            context.set(FLAG_C, 1)
        else:
            context.set(FLAG_C, 0)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'sub':
        carry = context.get(FLAG_C)
        val = context.get(src)
        now = context.get(dest)
        result = now - val - carry
        if result < 0:
            result &= 255
            context.set(FLAG_C, 1)
        else:
            context.set(FLAG_C, 0)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'inc':
        val = context.get(dest)
        result = (val + 1) & 255
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'dec':
        val = context.get(dest)
        result = (val - 1) & 255
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'cmp':
        val = context.get(src)
        now = context.get(dest)
        result = now - val
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        if result < 0:
            result &= 255
            context.set(FLAG_C, 1)
        else:
            context.set(FLAG_C, 0)
    elif opcode == 'and':
        result = context.get(dest) & context.get(src)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'or':
        result = context.get(dest) | context.get(src)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'xor':
        result = context.get(dest) ^ context.get(src)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'shl':
        val = context.get(dest)
        carry = context.get(FLAG_C)
        context.set(FLAG_C, 1 if val & 128 else 0)
        result = ((val << 1) + carry) & 255
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'shr':
        val = context.get(dest)
        carry = context.get(FLAG_C)
        context.set(FLAG_C, 1 if val & 1 else 0)
        result = (val >> 1) + (carry * 128)
        context.set(FLAG_Z, 1 if result == 0 else 0)
        context.set(FLAG_N, 1 if result & 128 else 0)
        context.set(dest, result)
    elif opcode == 'call':
        eval_routine(routines[instr.name], context, routines)
    elif opcode == 'if':
        val = context.get(src)
        test = (val != 0) if not instr.inverted else (val == 0)
        if test:
            eval_block(instr.block1, context, routines)
        elif instr.block2:
            eval_block(instr.block2, context, routines)
    elif opcode == 'repeat':
        eval_block(instr.block, context, routines)
        while context.get(src) == 0:
            eval_block(instr.block, context, routines)
    elif opcode == 'copy':
        context.set(dest, context.get(src))
        # these are trashed; so could be anything really
        context.set(REG_A, 0)
        context.set(FLAG_Z, 0)
        context.set(FLAG_N, 0)
    else:
        raise NotImplementedError
