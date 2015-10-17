# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    ConstantRef, LocationRef,
    REG_A, REG_X, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)
from sixtypical.emitter import Byte
from sixtypical.gen6502 import (
    Immediate, Absolute,
    LDA, LDX, LDY, STA, STX, STY, CLC, SEC, ADC, RTS
)


class UnsupportedOpcodeError(KeyError):
    pass


def compile_program(program, emitter):
    assert isinstance(program, Program)
    routines = {r.name: r for r in program.routines}
    for routine in program.routines:
        compile_routine(routine, emitter, routines)


def compile_routine(routine, emitter, routines):
    assert isinstance(routine, Routine)
    label = emitter.make_label(routine.name)
    if routine.block:
        compile_block(routine.block, emitter, routines)
        emitter.emit(RTS())
    return label


def compile_block(block, emitter, routines):
    assert isinstance(block, Block)
    label = emitter.make_label()
    for instr in block.instrs:
        compile_instr(instr, emitter, routines)
    return label


def compile_instr(instr, emitter, routines):
    assert isinstance(instr, Instr)
    opcode = instr.opcode
    dest = instr.dest
    src = instr.src

    if opcode == 'ld':
        if dest == REG_A:
            if isinstance(src, ConstantRef):
                emitter.emit(LDA(Immediate(Byte(src.value))))
            else:
                emitter.emit(LDA(Absolute(src.label)))
        elif dest == REG_X:
            pass
        elif dest == REG_Y:
            pass
        else:
            raise UnsupportedOpcodeError(instr)
    elif opcode == 'st':
        if dest == FLAG_C and src == ConstantRef(0):
            emitter.emit(CLC())
        elif dest == FLAG_C and src == ConstantRef(1):
            emitter.emit(SEC())
        elif src == REG_A:
            emitter.emit(STA(Absolute(dest.label)))
        elif src == REG_X:
            emitter.emit(STX(Absolute(dest.label)))
        elif src == REG_Y:
            emitter.emit(STY(Absolute(dest.label)))
        else:
            raise UnsupportedOpcodeError(instr)
    elif opcode == 'add':
        if dest == REG_A:
            if isinstance(src, ConstantRef):
                emitter.emit(ADC(Immediate(Byte(src.value))))
            else:
                emitter.emit(ADC(Absolute(src.label)))
        else:
            raise UnsupportedOpcodeError(instr)
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
        raise NotImplementedError(instr.name)
    elif opcode == 'if':
        raise NotImplementedError
    else:
        raise NotImplementedError
