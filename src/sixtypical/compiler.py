# Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
# This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
# SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical

# encoding: UTF-8

from sixtypical.ast import (
    Program, Routine, Block, SingleOp, Reset, Call, GoTo, If, Repeat, For, WithInterruptsOff, Save, PointInto
)
from sixtypical.model import (
    ConstantRef, LocationRef, IndexedRef, IndirectRef,
    TYPE_BIT, TYPE_BYTE, TYPE_WORD,
    TableType, PointerType, RoutineType, VectorType,
    REG_A, REG_X, REG_Y, FLAG_C
)
from sixtypical.emitter import Byte, Word, Table, Label, Offset, LowAddressByte, HighAddressByte
from sixtypical.gen6502 import (
    Immediate, Absolute, AbsoluteX, AbsoluteY, ZeroPage, Indirect, IndirectY, Relative,
    LDA, LDX, LDY, STA, STX, STY,
    TAX, TAY, TXA, TYA,
    PHA, PLA,
    CLC, SEC, ADC, SBC, ROL, ROR,
    INC, INX, INY, DEC, DEX, DEY,
    CMP, CPX, CPY, AND, ORA, EOR,
    BCC, BCS, BNE, BEQ, BPL, BMI,
    JMP, JSR, RTS,
    SEI, CLI,
    NOP,
)


class UnsupportedOpcodeError(KeyError):
    pass


class Compiler(object):
    def __init__(self, symtab, emitter):
        self.symtab = symtab
        self.emitter = emitter
        self.routines = {}           # routine.name -> Routine
        self.routine_locals = {}     # routine.name -> { local.name -> Label }
        self.labels = {}             # global.name -> Label  ("global" includes routines)
        self.trampolines = {}        # Location -> Label
        self.pointer_assoc = {}      # pointer name -> table name  (I'm not entirely happy about this)
        self.current_routine = None

    # - - - - helper methods - - - -

    def get_type_for_name(self, name):
        if self.current_routine and self.symtab.has_local(self.current_routine.name, name):
            return self.symtab.fetch_local_type(self.current_routine.name, name)
        return self.symtab.fetch_global_type(name)

    def get_type(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.type
        if not isinstance(ref, LocationRef):
            raise NotImplementedError
        return self.get_type_for_name(ref.name)

    def addressing_mode_for_index(self, index):
        if index == REG_X:
            return AbsoluteX
        elif index == REG_Y:
            return AbsoluteY
        else:
            raise NotImplementedError(index)

    def compute_length_of_defn(self, defn):
        length = None
        type_ = self.get_type_for_name(defn.name)
        if type_ == TYPE_BYTE:
            length = 1
        elif type_ == TYPE_WORD or isinstance(type_, (PointerType, VectorType)):
            length = 2
        elif isinstance(type_, TableType):
            length = type_.size * (1 if type_.of_type == TYPE_BYTE else 2)
        if length is None:
            raise NotImplementedError("Need size for type {}".format(type_))
        return length

    def get_label(self, name):
        if self.current_routine:
            local_label = self.routine_locals.get(self.current_routine.name, {}).get(name)
            if local_label:
                return local_label
        return self.labels[name]

    def absolute_or_zero_page(self, label):
        if label.addr is not None and label.addr < 256:
            return ZeroPage(label)
        else:
            return Absolute(label)

    # - - - - visitor methods - - - -

    def compile_program(self, program, compilation_roster=None):
        assert isinstance(program, Program)

        declarations = []

        for defn in program.defns:
            length = self.compute_length_of_defn(defn)
            label = Label(defn.name, addr=defn.addr, length=length)
            self.labels[defn.name] = label
            declarations.append((defn, self.symtab.fetch_global_type(defn.name), label))

        for routine in program.routines:
            self.routines[routine.name] = routine
            label = Label(routine.name)
            if routine.addr is not None:
                label.set_addr(routine.addr)
            self.labels[routine.name] = label

            self.current_routine = routine
            local_labels = {}
            for defn in routine.locals:
                length = self.compute_length_of_defn(defn)
                label = Label(defn.name, addr=defn.addr, length=length)
                local_labels[defn.name] = label
                declarations.append((defn, self.symtab.fetch_local_type(routine.name, defn.name), label))
            self.routine_locals[routine.name] = local_labels
            self.current_routine = None

        if compilation_roster is None:
            compilation_roster = [['main']] + [[routine.name] for routine in program.routines if routine.name != 'main']

        for roster_row in compilation_roster:
            for i, routine_name in enumerate(roster_row):
                if i < len(roster_row) - 1:
                    self.compile_routine(self.routines[routine_name], next_routine=self.routines[roster_row[i + 1]])
                else:
                    self.compile_routine(self.routines[routine_name])

        for location, label in self.trampolines.items():
            self.emitter.resolve_label(label)
            self.emitter.emit(JMP(Indirect(self.get_label(location.name))))
            self.emitter.emit(RTS())

        # initialized data
        for defn, type_, label in declarations:
            if defn.initial is not None:
                initial_data = None
                if type_ == TYPE_BYTE:
                    initial_data = Byte(defn.initial)
                elif type_ == TYPE_WORD:
                    initial_data = Word(defn.initial)
                elif TableType.is_a_table_type(type_, TYPE_BYTE):
                    initial_data = Table([Byte(i) for i in defn.initial], type_.size)
                elif TableType.is_a_table_type(type_, TYPE_WORD):
                    initial_data = Table([Word(i) for i in defn.initial], type_.size)
                else:
                    raise NotImplementedError(type_)
                label.set_length(initial_data.size())
                self.emitter.resolve_label(label)
                self.emitter.emit(initial_data)

        # uninitialized, "BSS" data
        for defn, type_, label in declarations:
            if defn.initial is None and defn.addr is None:
                self.emitter.resolve_bss_label(label)

    def compile_routine(self, routine, next_routine=None):
        assert isinstance(routine, Routine)

        self.current_routine = routine

        if routine.block:
            self.emitter.resolve_label(self.get_label(routine.name))
            self.compile_block(routine.block)

            needs_rts = True
            last_op = self.emitter.get_tail()

            if isinstance(last_op, JSR):
                if isinstance(last_op.operand, Absolute):
                    if isinstance(last_op.operand.value, Label):
                        label = last_op.operand.value
                        self.emitter.retract()
                        self.emitter.emit(JMP(Absolute(label)))
                        last_op = self.emitter.get_tail()

            if isinstance(last_op, JMP):
                needs_rts = False
                if isinstance(last_op.operand, Absolute):
                    if isinstance(last_op.operand.value, Label):
                        if next_routine and last_op.operand.value.name == next_routine.name:
                            self.emitter.retract()

            if needs_rts:
                self.emitter.emit(RTS())

        self.current_routine = None

    def compile_block(self, block):
        assert isinstance(block, Block)
        block.shallow_contains_goto = False
        for instr in block.instrs:
            self.compile_instr(instr)
            if isinstance(instr, GoTo):
                block.shallow_contains_goto = True

    def compile_instr(self, instr):
        if isinstance(instr, SingleOp):
            return self.compile_single_op(instr)
        elif isinstance(instr, Call):
            return self.compile_call(instr)
        elif isinstance(instr, GoTo):
            return self.compile_goto(instr)
        elif isinstance(instr, If):
            return self.compile_if(instr)
        elif isinstance(instr, Repeat):
            return self.compile_repeat(instr)
        elif isinstance(instr, For):
            return self.compile_for(instr)
        elif isinstance(instr, WithInterruptsOff):
            return self.compile_with_interrupts_off(instr)
        elif isinstance(instr, Save):
            return self.compile_save(instr)
        elif isinstance(instr, PointInto):
            return self.compile_point_into(instr)
        elif isinstance(instr, Reset):
            return self.compile_reset(instr)
        else:
            raise NotImplementedError

    def compile_single_op(self, instr):

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
                elif isinstance(src, IndexedRef) and src.index == REG_X:
                    self.emitter.emit(LDA(AbsoluteX(Offset(self.get_label(src.ref.name), src.offset.value))))
                elif isinstance(src, IndexedRef) and src.index == REG_Y:
                    self.emitter.emit(LDA(AbsoluteY(Offset(self.get_label(src.ref.name), src.offset.value))))
                elif isinstance(src, IndirectRef) and isinstance(self.get_type(src.ref), PointerType):
                    self.emitter.emit(LDA(IndirectY(self.get_label(src.ref.name))))
                else:
                    self.emitter.emit(LDA(self.absolute_or_zero_page(self.get_label(src.name))))
            elif dest == REG_X:
                if src == REG_A:
                    self.emitter.emit(TAX())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDX(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef) and src.index == REG_Y:
                    self.emitter.emit(LDX(AbsoluteY(Offset(self.get_label(src.ref.name), src.offset.value))))
                else:
                    self.emitter.emit(LDX(self.absolute_or_zero_page(self.get_label(src.name))))
            elif dest == REG_Y:
                if src == REG_A:
                    self.emitter.emit(TAY())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDY(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef) and src.index == REG_X:
                    self.emitter.emit(LDY(AbsoluteX(Offset(self.get_label(src.ref.name), src.offset.value))))
                else:
                    self.emitter.emit(LDY(self.absolute_or_zero_page(self.get_label(src.name))))
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

                if isinstance(dest, IndexedRef):
                    mode_cls = {
                        REG_X: AbsoluteX,
                        REG_Y: AbsoluteY,
                    }[dest.index]
                    operand = mode_cls(Offset(self.get_label(dest.ref.name), dest.offset.value))
                elif isinstance(dest, IndirectRef) and isinstance(self.get_type(dest.ref), PointerType):
                    operand = IndirectY(self.get_label(dest.ref.name))
                else:
                    operand = self.absolute_or_zero_page(self.get_label(dest.name))

                if op_cls is None:
                    raise UnsupportedOpcodeError(instr)
                self.emitter.emit(op_cls(operand))
        elif opcode == 'add':
            if dest == REG_X or dest == REG_Y:
                raise UnsupportedOpcodeError(instr)
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(ADC(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef):
                    mode = self.addressing_mode_for_index(src.index)
                    self.emitter.emit(ADC(mode(Offset(self.get_label(src.ref.name), src.offset.value))))
                else:
                    self.emitter.emit(ADC(Absolute(self.get_label(src.name))))
            elif isinstance(dest, LocationRef) and self.get_type(src) == TYPE_BYTE and self.get_type(dest) == TYPE_BYTE:
                if isinstance(src, ConstantRef):
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(ADC(Immediate(Byte(src.low_byte()))))
                    self.emitter.emit(STA(Absolute(dest_label)))
                elif isinstance(src, LocationRef):
                    src_label = self.get_label(src.name)
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(ADC(Absolute(src_label)))
                    self.emitter.emit(STA(Absolute(dest_label)))
                else:
                    raise UnsupportedOpcodeError(instr)
            elif isinstance(dest, LocationRef) and self.get_type(src) == TYPE_WORD and self.get_type(dest) == TYPE_WORD:
                if isinstance(src, ConstantRef):
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(ADC(Immediate(Byte(src.low_byte()))))
                    self.emitter.emit(STA(Absolute(dest_label)))
                    self.emitter.emit(LDA(Absolute(Offset(dest_label, 1))))
                    self.emitter.emit(ADC(Immediate(Byte(src.high_byte()))))
                    self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
                elif isinstance(src, LocationRef):
                    src_label = self.get_label(src.name)
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(ADC(Absolute(src_label)))
                    self.emitter.emit(STA(Absolute(dest_label)))
                    self.emitter.emit(LDA(Absolute(Offset(dest_label, 1))))
                    self.emitter.emit(ADC(Absolute(Offset(src_label, 1))))
                    self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
                else:
                    raise UnsupportedOpcodeError(instr)
            elif isinstance(dest, LocationRef) and self.get_type(src) == TYPE_WORD and isinstance(self.get_type(dest), PointerType):
                if isinstance(src, ConstantRef):
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(ZeroPage(dest_label)))
                    self.emitter.emit(ADC(Immediate(Byte(src.low_byte()))))
                    self.emitter.emit(STA(ZeroPage(dest_label)))
                    self.emitter.emit(LDA(ZeroPage(Offset(dest_label, 1))))
                    self.emitter.emit(ADC(Immediate(Byte(src.high_byte()))))
                    self.emitter.emit(STA(ZeroPage(Offset(dest_label, 1))))
                elif isinstance(src, LocationRef):
                    src_label = self.get_label(src.name)
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(ZeroPage(dest_label)))
                    self.emitter.emit(ADC(Absolute(src_label)))
                    self.emitter.emit(STA(ZeroPage(dest_label)))
                    self.emitter.emit(LDA(ZeroPage(Offset(dest_label, 1))))
                    self.emitter.emit(ADC(Absolute(Offset(src_label, 1))))
                    self.emitter.emit(STA(ZeroPage(Offset(dest_label, 1))))
                else:
                    raise UnsupportedOpcodeError(instr)
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'sub':
            if dest == REG_X or dest == REG_Y:
                raise UnsupportedOpcodeError(instr)
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(SBC(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef):
                    mode = self.addressing_mode_for_index(src.index)
                    self.emitter.emit(SBC(mode(Offset(self.get_label(src.ref.name), src.offset.value))))
                else:
                    self.emitter.emit(SBC(Absolute(self.get_label(src.name))))
            elif isinstance(dest, LocationRef) and self.get_type(src) == TYPE_BYTE and self.get_type(dest) == TYPE_BYTE:
                if isinstance(src, ConstantRef):
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(SBC(Immediate(Byte(src.low_byte()))))
                    self.emitter.emit(STA(Absolute(dest_label)))
                elif isinstance(src, LocationRef):
                    src_label = self.get_label(src.name)
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(SBC(Absolute(src_label)))
                    self.emitter.emit(STA(Absolute(dest_label)))
                else:
                    raise UnsupportedOpcodeError(instr)
            elif isinstance(dest, LocationRef) and self.get_type(src) == TYPE_WORD and self.get_type(dest) == TYPE_WORD:
                if isinstance(src, ConstantRef):
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(SBC(Immediate(Byte(src.low_byte()))))
                    self.emitter.emit(STA(Absolute(dest_label)))
                    self.emitter.emit(LDA(Absolute(Offset(dest_label, 1))))
                    self.emitter.emit(SBC(Immediate(Byte(src.high_byte()))))
                    self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
                elif isinstance(src, LocationRef):
                    src_label = self.get_label(src.name)
                    dest_label = self.get_label(dest.name)
                    self.emitter.emit(LDA(Absolute(dest_label)))
                    self.emitter.emit(SBC(Absolute(src_label)))
                    self.emitter.emit(STA(Absolute(dest_label)))
                    self.emitter.emit(LDA(Absolute(Offset(dest_label, 1))))
                    self.emitter.emit(SBC(Absolute(Offset(src_label, 1))))
                    self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
                else:
                    raise UnsupportedOpcodeError(instr)
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'cmp':
            self.compile_cmp(instr, instr.src, instr.dest)
        elif opcode in ('and', 'or', 'xor',):
            cls = {
                'and': AND,
                'or':  ORA,
                'xor': EOR,
            }[opcode]
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(cls(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef):
                    mode = self.addressing_mode_for_index(src.index)
                    self.emitter.emit(cls(mode(Offset(self.get_label(src.ref.name), src.offset.value))))
                else:
                    self.emitter.emit(cls(self.absolute_or_zero_page(self.get_label(src.name))))
            else:
                raise UnsupportedOpcodeError(instr)
        elif opcode == 'inc':
            self.compile_inc(instr, instr.dest)
        elif opcode == 'dec':
            self.compile_dec(instr, instr.dest)
        elif opcode in ('shl', 'shr'):
            cls = {
                'shl': ROL,
                'shr': ROR,
            }[opcode]
            if dest == REG_A:
                self.emitter.emit(cls())
            elif isinstance(dest, IndexedRef):
                mode = self.addressing_mode_for_index(dest.index)
                self.emitter.emit(cls(mode(Offset(self.get_label(dest.ref.name), dest.offset.value))))
            else:
                self.emitter.emit(cls(self.absolute_or_zero_page(self.get_label(dest.name))))
        elif opcode == 'copy':
            self.compile_copy(instr, instr.src, instr.dest)
        elif opcode == 'trash':
            pass
        elif opcode == 'nop':
            self.emitter.emit(NOP())
        else:
            raise NotImplementedError(opcode)

    def compile_call(self, instr):
        location = instr.location
        label = self.get_label(instr.location.name)
        location_type = self.get_type(location)
        if isinstance(location_type, RoutineType):
            self.emitter.emit(JSR(Absolute(label)))
        elif isinstance(location_type, VectorType):
            trampoline = self.trampolines.setdefault(
                location, Label(location.name + '_trampoline')
            )
            self.emitter.emit(JSR(Absolute(trampoline)))
        else:
            raise NotImplementedError(location_type)

    def compile_goto(self, instr):
        location = instr.location
        label = self.get_label(instr.location.name)
        location_type = self.get_type(location)
        if isinstance(location_type, RoutineType):
            self.emitter.emit(JMP(Absolute(label)))
        elif isinstance(location_type, VectorType):
            self.emitter.emit(JMP(Indirect(label)))
        else:
            raise NotImplementedError(location_type)

    def compile_cmp(self, instr, src, dest):
        """`instr` is only for reporting purposes"""
        if isinstance(src, LocationRef) and self.get_type(src) == TYPE_WORD:
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(dest_label)))
            self.emitter.emit(CMP(Absolute(src_label)))
            end_label = Label('end_label')
            self.emitter.emit(BNE(Relative(end_label)))
            self.emitter.emit(LDA(Absolute(Offset(dest_label, 1))))
            self.emitter.emit(CMP(Absolute(Offset(src_label, 1))))
            self.emitter.resolve_label(end_label)
            return
        if isinstance(src, ConstantRef) and self.get_type(src) == TYPE_WORD:
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(dest_label)))
            self.emitter.emit(CMP(Immediate(Byte(src.low_byte()))))
            end_label = Label('end_label')
            self.emitter.emit(BNE(Relative(end_label)))
            self.emitter.emit(LDA(Absolute(Offset(dest_label, 1))))
            self.emitter.emit(CMP(Immediate(Byte(src.high_byte()))))
            self.emitter.resolve_label(end_label)
            return
        cls = {
            'a': CMP,
            'x': CPX,
            'y': CPY,
        }.get(dest.name)
        if cls is None:
            raise UnsupportedOpcodeError(instr)
        if isinstance(src, ConstantRef):
            self.emitter.emit(cls(Immediate(Byte(src.value))))
        elif isinstance(src, IndexedRef):
            # FIXME might not work for some dest's (that is, cls's)
            mode = self.addressing_mode_for_index(src.index)
            self.emitter.emit(cls(mode(Offset(self.get_label(src.ref.name), src.offset.value))))
        else:
            self.emitter.emit(cls(Absolute(self.get_label(src.name))))

    def compile_inc(self, instr, dest):
        """`instr` is only for reporting purposes"""
        if dest == REG_X:
            self.emitter.emit(INX())
        elif dest == REG_Y:
            self.emitter.emit(INY())
        elif isinstance(dest, IndexedRef):
            mode = self.addressing_mode_for_index(dest.index)
            self.emitter.emit(INC(mode(Offset(self.get_label(dest.ref.name), dest.offset.value))))
        else:
            self.emitter.emit(INC(Absolute(self.get_label(dest.name))))

    def compile_dec(self, instr, dest):
        """`instr` is only for reporting purposes"""
        if dest == REG_X:
            self.emitter.emit(DEX())
        elif dest == REG_Y:
            self.emitter.emit(DEY())
        elif isinstance(dest, IndexedRef):
            mode = self.addressing_mode_for_index(dest.index)
            self.emitter.emit(DEC(mode(Offset(self.get_label(dest.ref.name), dest.offset.value))))
        else:
            self.emitter.emit(DEC(Absolute(self.get_label(dest.name))))

    def compile_copy(self, instr, src, dest):

        if isinstance(src, (IndirectRef, IndexedRef)):
            src_ref_type = self.get_type(src.ref)
        else:
            src_type = self.get_type(src)

        if isinstance(dest, (IndirectRef, IndexedRef)):
            dest_ref_type = self.get_type(dest.ref)
        else:
            dest_type = self.get_type(dest)

        if isinstance(src, ConstantRef) and isinstance(dest, IndirectRef) and src_type == TYPE_BYTE and isinstance(dest_ref_type, PointerType):
            ### copy 123, [ptr] + y
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Immediate(Byte(src.value))))
            self.emitter.emit(STA(IndirectY(dest_label)))
        elif isinstance(src, LocationRef) and isinstance(dest, IndirectRef) and src_type == TYPE_BYTE and isinstance(dest_ref_type, PointerType):
            ### copy b, [ptr] + y
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(IndirectY(dest_label)))
        elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef) and dest_type == TYPE_BYTE and isinstance(src_ref_type, PointerType):
            ### copy [ptr] + y, b
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(IndirectY(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
        elif isinstance(src, IndirectRef) and isinstance(dest, IndirectRef) and isinstance(src_ref_type, PointerType) and isinstance(dest_ref_type, PointerType):
            ### copy [ptra] + y, [ptrb] + y
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(IndirectY(src_label)))
            self.emitter.emit(STA(IndirectY(dest_label)))
        elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef) and src_type == TYPE_WORD and TableType.is_a_table_type(dest_ref_type, TYPE_WORD):
            ### copy w, wtab + y
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            mode = self.addressing_mode_for_index(dest.index)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value))))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value + 256))))
        elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef) and isinstance(src_type, VectorType) and isinstance(dest_ref_type, TableType) and isinstance(dest_ref_type.of_type, VectorType):
            ### copy vec, vtab + y
            # FIXME this is the exact same as above - can this be simplified?
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            mode = self.addressing_mode_for_index(dest.index)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value))))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value + 256))))
        elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef) and isinstance(src_type, RoutineType) and isinstance(dest_ref_type, TableType) and isinstance(dest_ref_type.of_type, VectorType):
            ### copy routine, vtab + y
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            mode = self.addressing_mode_for_index(dest.index)
            self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value))))
            self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value + 256))))
        elif isinstance(src, ConstantRef) and isinstance(dest, IndexedRef) and src_type == TYPE_WORD and TableType.is_a_table_type(dest_ref_type, TYPE_WORD):
            ### copy 9999, wtab + y
            dest_label = self.get_label(dest.ref.name)
            mode = self.addressing_mode_for_index(dest.index)
            self.emitter.emit(LDA(Immediate(Byte(src.low_byte()))))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value))))
            self.emitter.emit(LDA(Immediate(Byte(src.high_byte()))))
            self.emitter.emit(STA(mode(Offset(dest_label, dest.offset.value + 256))))
        elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef) and TableType.is_a_table_type(src_ref_type, TYPE_WORD) and dest_type == TYPE_WORD:
            ### copy wtab + y, w
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            mode = self.addressing_mode_for_index(src.index)
            self.emitter.emit(LDA(mode(Offset(src_label, src.offset.value))))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(mode(Offset(src_label, src.offset.value + 256))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef) and isinstance(dest_type, VectorType) and isinstance(src_ref_type, TableType) and isinstance(src_ref_type.of_type, VectorType):
            ### copy vtab + y, vec
            # FIXME this is the exact same as above - can this be simplified?
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            mode = self.addressing_mode_for_index(src.index)
            self.emitter.emit(LDA(mode(Offset(src_label, src.offset.value))))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(mode(Offset(src_label, src.offset.value + 256))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif src_type == TYPE_BYTE and dest_type == TYPE_BYTE and not isinstance(src, ConstantRef):
            ### copy b1, b2
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
        elif src_type == TYPE_WORD and dest_type == TYPE_WORD and isinstance(src, ConstantRef):
            ### copy 9999, w
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Immediate(Byte(src.low_byte()))))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Immediate(Byte(src.high_byte()))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif src_type == TYPE_WORD and dest_type == TYPE_WORD and not isinstance(src, ConstantRef):
            ### copy w1, w2
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif isinstance(src_type, VectorType) and isinstance(dest_type, VectorType):
            ### copy v1, v2
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif isinstance(src_type, RoutineType) and isinstance(dest_type, VectorType):
            ### copy routine, vec
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        else:
            raise NotImplementedError(src_type)

    def compile_if(self, instr):
        cls = {
            False: {
                'c': BCC,
                'z': BNE,
                'n': BPL,
            },
            True: {
                'c': BCS,
                'z': BEQ,
                'n': BMI,
            },
        }[instr.inverted].get(instr.src.name)
        if cls is None:
            raise UnsupportedOpcodeError(instr)
        else_label = Label('else_label')
        self.emitter.emit(cls(Relative(else_label)))
        self.compile_block(instr.block1)

        if instr.block2 is not None:
            if instr.block1.shallow_contains_goto:
                self.emitter.resolve_label(else_label)
                self.compile_block(instr.block2)
            else:
                end_label = Label('end_label')
                self.emitter.emit(JMP(Absolute(end_label)))
                self.emitter.resolve_label(else_label)
                self.compile_block(instr.block2)
                self.emitter.resolve_label(end_label)
        else:
            self.emitter.resolve_label(else_label)

    def compile_repeat(self, instr):
        top_label = self.emitter.make_label()
        self.compile_block(instr.block)
        if instr.src is None:  # indicates 'repeat forever'
            self.emitter.emit(JMP(Absolute(top_label)))
        else:
            cls = {
                False: {
                    'c': BCC,
                    'z': BNE,
                    'n': BPL,
                },
                True: {
                    'c': BCS,
                    'z': BEQ,
                    'n': BMI,
                },
            }[instr.inverted].get(instr.src.name)
            if cls is None:
                raise UnsupportedOpcodeError(instr)
            self.emitter.emit(cls(Relative(top_label)))

    def compile_for(self, instr):
        top_label = self.emitter.make_label()

        self.compile_block(instr.block)

        if instr.direction > 0:
            self.compile_inc(instr, instr.dest)
            final = instr.final.succ()
        elif instr.direction < 0:
            self.compile_dec(instr, instr.dest)
            final = instr.final.pred()
        self.compile_cmp(instr, final, instr.dest)
        self.emitter.emit(BNE(Relative(top_label)))

    def compile_with_interrupts_off(self, instr):
        self.emitter.emit(SEI())
        self.compile_block(instr.block)
        self.emitter.emit(CLI())

    def compile_save(self, instr):
        for location in instr.locations:
            if location == REG_A:
                self.emitter.emit(PHA())
            elif location == REG_X:
                self.emitter.emit(TXA())
                self.emitter.emit(PHA())
            elif location == REG_Y:
                self.emitter.emit(TYA())
                self.emitter.emit(PHA())
            else:
                src_label = self.get_label(location.name)
                self.emitter.emit(LDA(Absolute(src_label)))
                self.emitter.emit(PHA())

        self.compile_block(instr.block)

        for location in reversed(instr.locations):
            if location == REG_A:
                self.emitter.emit(PLA())
            elif location == REG_X:
                self.emitter.emit(PLA())
                self.emitter.emit(TAX())
            elif location == REG_Y:
                self.emitter.emit(PLA())
                self.emitter.emit(TAY())
            else:
                src_label = self.get_label(location.name)
                self.emitter.emit(PLA())
                self.emitter.emit(STA(Absolute(src_label)))

    def compile_point_into(self, instr):
        self.pointer_assoc[instr.pointer.name] = instr.table.name
        self.compile_block(instr.block)
        del self.pointer_assoc[instr.pointer.name]

    def compile_reset(self, instr):
        table_name = self.pointer_assoc[instr.pointer.name]
        src_label = Offset(self.get_label(table_name), instr.offset.value)
        dest_label = self.get_label(instr.pointer.name)

        self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
        self.emitter.emit(STA(ZeroPage(dest_label)))
        self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
        self.emitter.emit(STA(ZeroPage(Offset(dest_label, 1))))
