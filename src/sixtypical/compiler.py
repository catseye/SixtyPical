# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    ConstantRef, LocationRef,
    REG_A, REG_X, REG_Y, FLAG_Z, FLAG_N, FLAG_V, FLAG_C
)
from sixtypical.emitter import Label, Byte
from sixtypical.gen6502 import (
    Immediate, Absolute,
    LDA, LDX, LDY, STA, STX, STY, CLC, SEC, ADC, RTS, JSR
)


class UnsupportedOpcodeError(KeyError):
    pass


class Compiler(object):
    def __init__(self, emitter):
        self.emitter = emitter
        self.routines = {}
        self.labels = {}

    def compile_program(self, program):
        assert isinstance(program, Program)
        for routine in program.routines:
            self.routines[routine.name] = routine
            label = Label(routine.name)
            if routine.addr is not None:
                label.set_addr(routine.addr)
            self.labels[routine.name] = label
        for routine in program.routines:
            self.compile_routine(routine)

    def compile_routine(self, routine):
        assert isinstance(routine, Routine)
        if routine.block:
            self.compile_block(routine.block)
            self.emitter.emit(RTS())

    def compile_block(self, block):
        assert isinstance(block, Block)
        label = self.emitter.make_label()
        for instr in block.instrs:
            self.compile_instr(instr)
        return label

    def compile_instr(self, instr):
        assert isinstance(instr, Instr)
        opcode = instr.opcode
        dest = instr.dest
        src = instr.src
    
        if opcode == 'ld':
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(LDA(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(LDA(Absolute(src.label)))
            elif dest == REG_X:
                pass
            elif dest == REG_Y:
                pass
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'st':
            if dest == FLAG_C and src == ConstantRef(0):
                self.emitter.emit(CLC())
            elif dest == FLAG_C and src == ConstantRef(1):
                self.emitter.emit(SEC())
            elif src == REG_A:
                self.emitter.emit(STA(Absolute(dest.label)))
            elif src == REG_X:
                self.emitter.emit(STX(Absolute(dest.label)))
            elif src == REG_Y:
                self.emitter.emit(STY(Absolute(dest.label)))
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'add':
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(ADC(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(ADC(Absolute(src.label)))
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
            label = self.labels[instr.name]
            self.emitter.emit(JSR(Absolute(label)))
        elif opcode == 'if':
            raise NotImplementedError
        else:
            raise NotImplementedError
