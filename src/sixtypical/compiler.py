# encoding: UTF-8

from sixtypical.ast import Program, Routine, Block, Instr, SingleOp, If, Repeat, For, WithInterruptsOff
from sixtypical.model import (
    ConstantRef, LocationRef, IndexedRef, IndirectRef, AddressRef,
    TYPE_BIT, TYPE_BYTE, TYPE_WORD,
    TableType, BufferType, PointerType, RoutineType, VectorType,
    REG_A, REG_X, REG_Y, FLAG_C
)
from sixtypical.emitter import Byte, Word, Table, Label, Offset, LowAddressByte, HighAddressByte
from sixtypical.gen6502 import (
    Immediate, Absolute, AbsoluteX, AbsoluteY, ZeroPage, Indirect, IndirectY, Relative,
    LDA, LDX, LDY, STA, STX, STY,
    TAX, TAY, TXA, TYA,
    CLC, SEC, ADC, SBC, ROL, ROR,
    INC, INX, INY, DEC, DEX, DEY,
    CMP, CPX, CPY, AND, ORA, EOR,
    BCC, BCS, BNE, BEQ,
    JMP, JSR, RTS,
    SEI, CLI,
    NOP,
)


class UnsupportedOpcodeError(KeyError):
    pass


class Compiler(object):
    def __init__(self, emitter):
        self.emitter = emitter
        self.routines = {}           # routine.name -> Routine
        self.routine_statics = {}    # routine.name -> { static.name -> Label }
        self.labels = {}             # global.name -> Label  ("global" includes routines)
        self.trampolines = {}        # Location -> Label
        self.current_routine = None

    # helper methods

    def addressing_mode_for_index(self, index):
        if index == REG_X:
            return AbsoluteX
        elif index == REG_Y:
            return AbsoluteY
        else:
            raise NotImplementedError(index)

    def compute_length_of_defn(self, defn):
        length = None
        type_ = defn.location.type
        if type_ == TYPE_BYTE:
            length = 1
        elif type_ == TYPE_WORD or isinstance(type_, (PointerType, VectorType)):
            length = 2
        elif isinstance(type_, TableType):
            length = type_.size * (1 if type_.of_type == TYPE_BYTE else 2)
        elif isinstance(type_, BufferType):
            length = type_.size
        if length is None:
            raise NotImplementedError("Need size for type {}".format(type_))
        return length

    def get_label(self, name):
        if self.current_routine:
            static_label = self.routine_statics.get(self.current_routine.name, {}).get(name)
            if static_label:
                return static_label
        return self.labels[name]

    def absolute_or_zero_page(self, label):
        if label.addr is not None and label.addr < 256:
            return ZeroPage(label)
        else:
            return Absolute(label)

    # visitor methods

    def compile_program(self, program):
        assert isinstance(program, Program)

        defn_labels = []

        for defn in program.defns:
            length = self.compute_length_of_defn(defn)
            label = Label(defn.name, addr=defn.addr, length=length)
            self.labels[defn.name] = label
            defn_labels.append((defn, label))

        for routine in program.routines:
            self.routines[routine.name] = routine
            label = Label(routine.name)
            if routine.addr is not None:
                label.set_addr(routine.addr)
            self.labels[routine.name] = label

            if hasattr(routine, 'statics'):
                static_labels = {}
                for defn in routine.statics:
                    length = self.compute_length_of_defn(defn)
                    label = Label(defn.name, addr=defn.addr, length=length)
                    static_labels[defn.name] = label
                    defn_labels.append((defn, label))
                self.routine_statics[routine.name] = static_labels

        self.compile_routine(self.routines['main'])
        for routine in program.routines:
            if routine.name != 'main':
                self.compile_routine(routine)

        for location, label in self.trampolines.iteritems():
            self.emitter.resolve_label(label)
            self.emitter.emit(JMP(Indirect(self.get_label(location.name))))
            self.emitter.emit(RTS())

        # initialized data
        for defn, label in defn_labels:
            if defn.initial is not None:
                initial_data = None
                type_ = defn.location.type
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
        for defn, label in defn_labels:
            if defn.initial is None and defn.addr is None:
                self.emitter.resolve_bss_label(label)

    def compile_routine(self, routine):
        self.current_routine = routine
        assert isinstance(routine, Routine)
        if routine.block:
            self.emitter.resolve_label(self.get_label(routine.name))
            self.compile_block(routine.block)
            self.emitter.emit(RTS())
        self.current_routine = None

    def compile_block(self, block):
        assert isinstance(block, Block)
        for instr in block.instrs:
            self.compile_instr(instr)

    def compile_instr(self, instr):
        if isinstance(instr, SingleOp):
            return self.compile_single_op(instr)
        elif isinstance(instr, If):
            return self.compile_if(instr)
        elif isinstance(instr, Repeat):
            return self.compile_repeat(instr)
        elif isinstance(instr, For):
            return self.compile_for(instr)
        elif isinstance(instr, WithInterruptsOff):
            return self.compile_with_interrupts_off(instr)
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
                    self.emitter.emit(LDA(AbsoluteX(self.get_label(src.ref.name))))
                elif isinstance(src, IndexedRef) and src.index == REG_Y:
                    self.emitter.emit(LDA(AbsoluteY(self.get_label(src.ref.name))))
                elif isinstance(src, IndirectRef) and isinstance(src.ref.type, PointerType):
                    self.emitter.emit(LDA(IndirectY(self.get_label(src.ref.name))))
                else:
                    self.emitter.emit(LDA(self.absolute_or_zero_page(self.get_label(src.name))))
            elif dest == REG_X:
                if src == REG_A:
                    self.emitter.emit(TAX())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDX(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef) and src.index == REG_Y:
                    self.emitter.emit(LDX(AbsoluteY(self.get_label(src.ref.name))))
                else:
                    self.emitter.emit(LDX(self.absolute_or_zero_page(self.get_label(src.name))))
            elif dest == REG_Y:
                if src == REG_A:
                    self.emitter.emit(TAY())
                elif isinstance(src, ConstantRef):
                    self.emitter.emit(LDY(Immediate(Byte(src.value))))
                elif isinstance(src, IndexedRef) and src.index == REG_X:
                    self.emitter.emit(LDY(AbsoluteX(self.get_label(src.ref.name))))
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
                    operand = mode_cls(self.get_label(dest.ref.name))
                elif isinstance(dest, IndirectRef) and isinstance(dest.ref.type, PointerType):
                    operand = IndirectY(self.get_label(dest.ref.name))
                else:
                    operand = self.absolute_or_zero_page(self.get_label(dest.name))

                if op_cls is None:
                    raise UnsupportedOpcodeError(instr)
                self.emitter.emit(op_cls(operand))
        elif opcode == 'add':
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(ADC(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(ADC(Absolute(self.get_label(src.name))))
            elif isinstance(dest, LocationRef) and src.type == TYPE_WORD and dest.type == TYPE_WORD:
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
            elif isinstance(dest, LocationRef) and src.type == TYPE_WORD and isinstance(dest.type, PointerType):
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
            if dest == REG_A:
                if isinstance(src, ConstantRef):
                    self.emitter.emit(SBC(Immediate(Byte(src.value))))
                else:
                    self.emitter.emit(SBC(Absolute(self.get_label(src.name))))
            elif isinstance(dest, LocationRef) and src.type == TYPE_WORD and dest.type == TYPE_WORD:
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
        elif opcode == 'inc':
            self.compile_inc(instr, instr.dest)
        elif opcode == 'dec':
            self.compile_dec(instr, instr.dest)
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
                else:
                    self.emitter.emit(cls(Absolute(self.get_label(src.name))))
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
            label = self.get_label(instr.location.name)
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
            label = self.get_label(instr.location.name)
            if isinstance(location.type, RoutineType):
                self.emitter.emit(JMP(Absolute(label)))
            elif isinstance(location.type, VectorType):
                self.emitter.emit(JMP(Indirect(label)))
            else:
                raise NotImplementedError
        elif opcode == 'copy':
            self.compile_copy(instr, instr.src, instr.dest)
        elif opcode == 'trash':
            pass
        elif opcode == 'nop':
            self.emitter.emit(NOP())
        else:
            raise NotImplementedError(opcode)

    def compile_cmp(self, instr, src, dest):
        """`instr` is only for reporting purposes"""
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
            self.emitter.emit(cls(Absolute(self.get_label(src.name))))

    def compile_inc(self, instr, dest):
        """`instr` is only for reporting purposes"""
        if dest == REG_X:
            self.emitter.emit(INX())
        elif dest == REG_Y:
            self.emitter.emit(INY())
        else:
            self.emitter.emit(INC(Absolute(self.get_label(dest.name))))

    def compile_dec(self, instr, dest):
        """`instr` is only for reporting purposes"""
        if dest == REG_X:
            self.emitter.emit(DEX())
        elif dest == REG_Y:
            self.emitter.emit(DEY())
        else:
            self.emitter.emit(DEC(Absolute(self.get_label(dest.name))))

    def compile_copy(self, instr, src, dest):
        if isinstance(src, ConstantRef) and isinstance(dest, IndirectRef) and src.type == TYPE_BYTE and isinstance(dest.ref.type, PointerType):
            ### copy 123, [ptr] + y
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Immediate(Byte(src.value))))
            self.emitter.emit(STA(IndirectY(dest_label)))
        elif isinstance(src, LocationRef) and isinstance(dest, IndirectRef) and src.type == TYPE_BYTE and isinstance(dest.ref.type, PointerType):
            ### copy b, [ptr] + y
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(IndirectY(dest_label)))
        elif isinstance(src, IndirectRef) and isinstance(dest, LocationRef) and dest.type == TYPE_BYTE and isinstance(src.ref.type, PointerType):
            ### copy [ptr] + y, b
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(IndirectY(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
        elif isinstance(src, AddressRef) and isinstance(dest, LocationRef) and isinstance(src.ref.type, BufferType) and isinstance(dest.type, PointerType):
            ### copy ^buf, ptr
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
            self.emitter.emit(STA(ZeroPage(dest_label)))
            self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
            self.emitter.emit(STA(ZeroPage(Offset(dest_label, 1))))
        elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef) and src.type == TYPE_WORD and TableType.is_a_table_type(dest.ref.type, TYPE_WORD):
            ### copy w, wtab + y
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(dest_label)))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(Offset(dest_label, 256))))
        elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef) and isinstance(src.type, VectorType) and isinstance(dest.ref.type, TableType) and isinstance(dest.ref.type.of_type, VectorType):
            ### copy vec, vtab + y
            # FIXME this is the exact same as above - can this be simplified?
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(dest_label)))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(Offset(dest_label, 256))))
        elif isinstance(src, LocationRef) and isinstance(dest, IndexedRef) and isinstance(src.type, RoutineType) and isinstance(dest.ref.type, TableType) and isinstance(dest.ref.type.of_type, VectorType):
            ### copy routine, vtab + y
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(dest_label)))
            self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(Offset(dest_label, 256))))
        elif isinstance(src, ConstantRef) and isinstance(dest, IndexedRef) and src.type == TYPE_WORD and TableType.is_a_table_type(dest.ref.type, TYPE_WORD):
            ### copy 9999, wtab + y
            dest_label = self.get_label(dest.ref.name)
            self.emitter.emit(LDA(Immediate(Byte(src.low_byte()))))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(dest_label)))
            self.emitter.emit(LDA(Immediate(Byte(src.high_byte()))))
            self.emitter.emit(STA(self.addressing_mode_for_index(dest.index)(Offset(dest_label, 256))))
        elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef) and TableType.is_a_table_type(src.ref.type, TYPE_WORD) and dest.type == TYPE_WORD:
            ### copy wtab + y, w
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(self.addressing_mode_for_index(src.index)(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(self.addressing_mode_for_index(src.index)(Offset(src_label, 256))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif isinstance(src, IndexedRef) and isinstance(dest, LocationRef) and isinstance(dest.type, VectorType) and isinstance(src.ref.type, TableType) and isinstance(src.ref.type.of_type, VectorType):
            ### copy vtab + y, vec
            # FIXME this is the exact same as above - can this be simplified?
            src_label = self.get_label(src.ref.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(self.addressing_mode_for_index(src.index)(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(self.addressing_mode_for_index(src.index)(Offset(src_label, 256))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif src.type == TYPE_BYTE and dest.type == TYPE_BYTE and not isinstance(src, ConstantRef):
            ### copy b1, b2
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
        elif src.type == TYPE_WORD and dest.type == TYPE_WORD and isinstance(src, ConstantRef):
            ### copy 9999, w
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Immediate(Byte(src.low_byte()))))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Immediate(Byte(src.high_byte()))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif src.type == TYPE_WORD and dest.type == TYPE_WORD and not isinstance(src, ConstantRef):
            ### copy w1, w2
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif isinstance(src.type, VectorType) and isinstance(dest.type, VectorType):
            ### copy v1, v2
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Absolute(src_label)))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Absolute(Offset(src_label, 1))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        elif isinstance(src.type, RoutineType) and isinstance(dest.type, VectorType):
            ### copy routine, vec
            src_label = self.get_label(src.name)
            dest_label = self.get_label(dest.name)
            self.emitter.emit(LDA(Immediate(HighAddressByte(src_label))))
            self.emitter.emit(STA(Absolute(dest_label)))
            self.emitter.emit(LDA(Immediate(LowAddressByte(src_label))))
            self.emitter.emit(STA(Absolute(Offset(dest_label, 1))))
        else:
            raise NotImplementedError(src.type)

    def compile_if(self, instr):
        cls = {
            False: {
                'c': BCC,
                'z': BNE,
            },
            True: {
                'c': BCS,
                'z': BEQ,
            },
        }[instr.inverted].get(instr.src.name)
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
                },
                True: {
                    'c': BCS,
                    'z': BEQ,
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
