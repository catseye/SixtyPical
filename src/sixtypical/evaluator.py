# encoding: UTF-8

from sixtypical.ast import Program, Defn, Routine, Block, Instr
from sixtypical.parser import ConstantRef, LocationRef


# TODO: should not inherit from dict
class Context(dict):
    def get(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        elif isinstance(ref, LocationRef):
            return self[ref.name]
        else:
            raise ValueError(ref)

    def set(self, ref, value):
        assert isinstance(ref, LocationRef)
        self[ref.name] = value


def eval_program(program):
    assert isinstance(program, Program)
    routines = {r.name: r for r in program.routines}
    context = Context({
        'a': 0, 'x': 0, 'y': 0,
        'c': 0, 'n': 0, 'z': 0, 'v': 0
    })
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
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'st':
        context.set(dest, context.get(src))
    elif opcode == 'add':
        carry = context['c']
        val = context.get(src)
        now = context.get(dest)
        result = now + val + carry
        if result > 255:
            result &= 255
            context['c'] = 1
        else:
            context['c'] = 0
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'sub':
        carry = context['c']
        val = context.get(src)
        now = context.get(dest)
        result = now - val - carry
        if result < 0:
            result &= 255
            context['c'] = 1
        else:
            context['c'] = 0
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'inc':
        val = context.get(dest)
        result = (val + 1) & 255
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'dec':
        val = context.get(dest)
        result = (val - 1) & 255
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'cmp':
        val = context.get(src)
        now = context.get(dest)
        result = now - val
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        if result < 0:
            result &= 255
            context['c'] = 1
        else:
            context['c'] = 0
    elif opcode == 'and':
        result = context.get(dest) & context.get(src)
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'or':
        result = context.get(dest) | context.get(src)
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'xor':
        result = context.get(dest) ^ context.get(src)
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'shl':
        val = context.get(dest)
        carry = context['c']
        context['c'] = 1 if val & 128 else 0
        result = ((val << 1) + carry) & 255
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'shr':
        val = context.get(dest)
        carry = context['c']
        context['c'] = 1 if val & 1 else 0
        result = (val >> 1) + (carry * 128)
        context['z'] = 1 if result == 0 else 0
        context['n'] = 1 if result & 128 else 0
        context.set(dest, result)
    elif opcode == 'call':
        eval_routine(routines[instr.name], context, routines)
    elif opcode == 'if':
        val = context.get(src)
        if val != 0:
            eval_block(instr.block1, context, routines)
        elif instr.block2:
            eval_block(instr.block2, context, routines)
    else:
        raise NotImplementedError
