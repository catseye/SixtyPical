# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr
from sixtypical.model import (
    ConstantRef,
    TYPE_BIT,
    RoutineType, VectorType,
    REG_A, REG_X, REG_Y, FLAG_C
)
from sixtypical.emitter import Byte, Label, Offset, LowAddressByte, HighAddressByte
from sixtypical.gen6502 import (
    Immediate, Absolute, AbsoluteX, AbsoluteY, Indirect, Relative,
    LDA, LDX, LDY, STA, STX, STY,
    TAX, TAY, TXA, TYA,
    CLC, SEC, ADC, SBC, ROL, ROR,
    INC, INX, INY, DEC, DEX, DEY,
    CMP, CPX, CPY, AND, ORA, EOR,
    BCC, BCS, BNE, BEQ,
    JMP, JSR, RTS,
    SEI, CLI,
)


class UnsupportedOpcodeError(KeyError):
    pass


class Compiler(object):
    def __init__(self, emitter):
        self.emitter = emitter
        self.routines = {}
        self.labels = {}
        self.trampolines = {}   # Location -> Label

    def compile_program(self, program):
        assert isinstance(program, Program)

        for defn in program.defns:
            label = Label(defn.name)
            if defn.addr is not None:
                label.set_addr(defn.addr)
            self.labels[defn.name] = label

        for routine in program.routines:
            self.routines[routine.name] = routine
            label = Label(routine.name)
            if routine.addr is not None:
                label.set_addr(routine.addr)
            self.labels[routine.name] = label

        self.compile_routine(self.routines['main'])
        for routine in program.routines:
            if routine.name != 'main':
                self.compile_routine(routine)

        for location, label in self.trampolines.iteritems():
            self.emitter.resolve_label(label)
            self.emitter.emit(JMP(Indirect(self.labels[location.name])))

        for defn in program.defns:
            if defn.addr is None:
                label = self.labels[defn.name]
                self.emitter.resolve_bss_label(label)


    def compile_routine(self, routine):
        assert isinstance(routine, Routine)
        if routine.block:
            self.emitter.resolve_label(self.labels[routine.name])
            self.compile_block(routine.block)
            self.emitter.emit(RTS())

    def compile_block(self, block):
        assert isinstance(block, Block)
        for instr in block.instrs:
            self.compile_instr(instr)

    def compile_instr(self, instr):
        assert isinstance(instr, Instr)
        opcode = instr.opcode
        dest = instr.dest
        src = instr.src
    
        if opcode == 'ld':
            if dest == REG_A:
                if src == REG_X:
                    self.emitter.emit(TXA())
                elif src == REG_Y:
                    self.emitter.emit(TYA())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDA(Immediate(Byte(src.value))))
                elif instr.index == REG_X:
                    self.emitter.emit(LDA(AbsoluteX(self.labels[src.name])))
                elif instr.index == REG_Y:
                    self.emitter.emit(LDA(AbsoluteY(self.labels[src.name])))
                else:
                    self.emitter.emit(LDA(Absolute(self.labels[src.name])))
            elif dest == REG_X:
                if src == REG_A:
                    self.emitter.emit(TAX())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDX(Immediate(Byte(src.value))))
                elif instr.index == REG_Y:
                    self.emitter.emit(LDX(AbsoluteY(self.labels[src.name])))
                else:
                    self.emitter.emit(LDX(Absolute(self.labels[src.name])))
            elif dest == REG_Y:
                if src == REG_A:
                    self.emitter.emit(TAY())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDY(Immediate(Byte(src.value))))
                elif instr.index == REG_X:
                    self.emitter.emit(LDY(AbsoluteX(self.labels[src.name])))
                else:
                    self.emitter.emit(LDY(Absolute(self.labels[src.name])))
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'st':
            if dest == FLAG_C and src == ConstantRef(TYPE_BIT, 0):
                self.emitter.emit(CLC())
            elif dest == FLAG_C and src == ConstantRef(TYPE_BIT, 1):
                self.emitter.emit(SEC())
            else:
                op_cls = {
                    REG_A: STA,
                    REG_X: STX,
                    REG_Y: STY
                }.get(src, None)
                mode_cls = {
                    REG_X: AbsoluteX,
                    REG_Y: AbsoluteY,
                    None: Absolute
                }.get(instr.index, None)
                if op_cls is None or mode_cls is None:
                    raise UnsupportedOpcodeError(instr)
                self.emitter.emit(op_cls(mode_cls(self.labels[dest.name])))
        elif opcode == 'add':
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(ADC(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(ADC(Absolute(self.labels[src.name])))
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'sub':
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(SBC(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(SBC(Absolute(self.labels[src.name])))
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'inc':
            if dest == REG_X:
                self.emitter.emit(INX())
            elif dest == REG_Y:
                self.emitter.emit(INY())
            else:
                self.emitter.emit(INC(Absolute(self.labels[dest.name])))
        elif opcode == 'dec':
            if dest == REG_X:
                self.emitter.emit(DEX())
            elif dest == REG_Y:
                self.emitter.emit(DEY())
            else:
                self.emitter.emit(DEC(Absolute(self.labels[dest.name])))
        elif opcode == 'cmp':
            cls = {
                'a': CMP,
                'x': CPX,
                'y': CPY,
            }.get(dest.name)
            if cls is None:
                raise UnsupportedOpcodeError(instr)
            if isinstance(src, ConstantRef):
                self.emitter.emit(cls(Immediate(Byte(src.value))))
            else:
                self.emitter.emit(cls(Absolute(self.labels[src.name])))
        elif opcode in ('and', 'or', 'xor',):
            cls = {
                'and': AND,
                'or':  ORA,
                'xor': EOR,
            }[opcode]
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(cls(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(cls(Absolute(self.labels[src.name])))
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode in ('shl', 'shr'):
            cls = {
                'shl': ROL,
                'shr': ROR,
            }[opcode]
            if dest == REG_A:
                self.emitter.emit(cls())
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'call':
            location = instr.location
            label = self.labels[instr.location.name]
            if isinstance(location.type, RoutineType):
                self.emitter.emit(JSR(Absolute(label)))
            elif isinstance(location.type, VectorType):
                trampoline = self.trampolines.setdefault(
                    location, Label(location.name + '_trampoline')
                )
                self.emitter.emit(JSR(Absolute(trampoline)))
            else:
                raise NotImplementedError
        elif opcode == 'goto':
            location = instr.location
            label = self.labels[instr.location.name]
            if isinstance(location.type, RoutineType):
                self.emitter.emit(JMP(Absolute(label)))
            elif isinstance(location.type, VectorType):
                self.emitter.emit(JMP(Indirect(label)))
            else:
                raise NotImplementedError
        elif opcode == 'if':
            cls = {
                False: {
                    'c': BCC,
                    'z': BNE,
                },
                True: {
                    'c': BCS,
                    'z': BEQ,
                },
            }[instr.inverted].get(src.name)
            if cls is None:
                raise UnsupportedOpcodeError(instr)
            else_label = Label('else_label')
            self.emitter.emit(cls(Relative(else_label)))
            self.compile_block(instr.block1)
            if instr.block2 is not None:
                end_label = Label('end_label')
                self.emitter.emit(JMP(Absolute(end_label)))
                self.emitter.resolve_label(else_label)
                self.compile_block(instr.block2)
                self.emitter.resolve_label(end_label)
            else:
                self.emitter.resolve_label(else_label)
        elif opcode == 'repeat':
            top_label = self.emitter.make_label()
            self.compile_block(instr.block)
            if src is None:
                self.emitter.emit(JMP(Absolute(top_label)))
            else:
                cls = {
                    False: {
                        'c': BCC,
                        'z': BNE,
                    },
                    True: {
                        'c': BCS,
                        'z': BEQ,
                    },
                }[instr.inverted].get(src.name)
                if cls is None:
                    raise UnsupportedOpcodeError(instr)
                self.emitter.emit(cls(Relative(top_label)))
        elif opcode == 'with-sei':
            self.emitter.emit(SEI())
            self.compile_block(instr.block)
            self.emitter.emit(CLI())
        elif opcode == 'copy':
            if isinstance(src.type, VectorType) and isinstance(dest.type, VectorType):
                src_label = self.labels[src.name]
                dest_label = self.labels[dest.name]
                self.emitter.emit(LDA(Absolute(src_label)))
                self.emitter.emit(STA(Absolute(dest_label)))
                self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
                self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
            elif isinstance(src.type, RoutineType) and isinstance(dest.type, VectorType):
                src_label = self.labels[src.name]
                dest_label = self.labels[dest.name]
                self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
                self.emitter.emit(STA(Absolute(dest_label)))
                self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
                self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
            else:
                raise NotImplementedError(src.type)
        else:
            raise NotImplementedError(opcode)
