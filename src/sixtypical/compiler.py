# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    ConstantRef, LocationRef,
    REG_A, REG_X, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)
from sixtypical.gen6502 import Generator


def compile_program(program, emitter):
    assert isinstance(program, Program)
    generator = Generator(emitter)
    routines = {r.name: r for r in program.routines}
    for routine in program.routines:
        compile_routine(routine, generator, routines)


def compile_routine(routine, generator, routines):
    assert isinstance(routine, Routine)
    label = generator.emitter.make_label(routine.name)
    compile_block(routine.block, generator, routines)
    return label


def compile_block(block, generator, routines):
    assert isinstance(block, Block)
    label = generator.emitter.make_label()
    for instr in block.instrs:
        compile_instr(instr, generator, routines)
    return label


def compile_instr(instr, generator, routines):
    assert isinstance(instr, Instr)
    opcode = instr.opcode
    dest = instr.dest
    src = instr.src

    if opcode == 'ld':
        if dest == REG_A:
            if isinstance(src, ConstantRef):
                # LDA #...
                pass
            else:
                # LDA abs
                pass
        elif dest == REG_X:
            pass
        elif dest == REG_Y:
            pass
        else:
            raise KeyError
    elif opcode == 'st':
        if src == REG_A:
            # assert isinstance(dest, MemoryRef)
            # generate STA
            pass
        else:
            raise KeyError
    elif opcode == 'add':
        raise NotImplementedError
    elif opcode == 'sub':
        raise NotImplementedError
    elif opcode == 'inc':
        raise NotImplementedError
    elif opcode == 'dec':
        raise NotImplementedError
    elif opcode == 'cmp':
        raise NotImplementedError
    elif opcode == 'and':
        raise NotImplementedError
    elif opcode == 'or':
        raise NotImplementedError
    elif opcode == 'xor':
        raise NotImplementedError
    elif opcode == 'shl':
        raise NotImplementedError
    elif opcode == 'shr':
        raise NotImplementedError
    elif opcode == 'call':
        raise NotImplementedError
    elif opcode == 'if':
        raise NotImplementedError
    else:
        raise NotImplementedError
