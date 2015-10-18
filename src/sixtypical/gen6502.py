"""Emittables for 6502 machine code."""

from sixtypical.emitter import Emittable, Byte, Word, Label


class AddressingMode(object):
    def size(self):
        """Size of the operand for the mode (not including the opcode)"""
        raise NotImplementedError

    def serialize(self, addr):
        raise NotImplementedError

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.value)


class Implied(AddressingMode):
    def size(self):
        return 0

    def serialize(self, addr):
        return ''

    def __repr__(self):
        return "%s()" % (self.__class__.__name__)


class Immediate(AddressingMode):
    def __init__(self, value):
        assert isinstance(value, Byte)
        self.value = value

    def size(self):
        return 1

    def serialize(self, addr):
        return self.value.serialize(addr)


class Absolute(AddressingMode):
    def __init__(self, value):
        assert isinstance(value, Label)
        self.value = value

    def size(self):
        return 2

    def serialize(self, addr):
        return self.value.serialize(addr)


class AbsoluteX(Absolute):
    pass


class AbsoluteY(Absolute):
    pass


class Relative(AddressingMode):
    def __init__(self, value):
        assert isinstance(value, Label)
        self.value = value

    def size(self):
        return 1

    def serialize(self, addr):
        return self.value.serialize_relative_to(addr)


class Opcode(Emittable):
    def __init__(self, operand=None):
        self.operand = operand or Implied()

    def size(self):
        return 1 + self.operand.size() if self.operand else 0

    def serialize(self, addr):
        return (
            chr(self.opcodes[self.operand.__class__]) +
            self.operand.serialize(addr)
        )

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.operand)


class ADC(Opcode):
    opcodes = {
        Immediate: 0x69,
        Absolute:  0x6d,
        AbsoluteX: 0x7d,
        AbsoluteY: 0x79,
    }


class AND(Opcode):
    opcodes = {
        Immediate: 0x29,
        Absolute:  0x2d,
        AbsoluteX: 0x3d,
        AbsoluteY: 0x39,
    }


class BCC(Opcode):
    opcodes = {
        Relative:  0x90,
    }


class BCS(Opcode):
    opcodes = {
        Relative:  0xb0,
    }


class BEQ(Opcode):
    opcodes = {
        Relative:  0xf0,
    }


class BNE(Opcode):
    opcodes = {
        Relative:  0xd0,
    }


class CLC(Opcode):
    opcodes = {
        Implied:   0x18
    }


class CLI(Opcode):
    opcodes = {
        Implied:   0x58,
    }


class CMP(Opcode):
    opcodes = {
        Immediate: 0xc9,
        Absolute:  0xcd,
        AbsoluteX: 0xdd,
        AbsoluteY: 0xd9,
    }


class CPX(Opcode):
    opcodes = {
        Immediate: 0xe0,
        Absolute:  0xec,
    }


class CPY(Opcode):
    opcodes = {
        Immediate: 0xc0,
        Absolute:  0xcc,
    }


class DEC(Opcode):
    opcodes = {
        Absolute:  0xce,
    }


class DEX(Opcode):
    opcodes = {
        Implied:   0xca,
    }


class DEY(Opcode):
    opcodes = {
        Implied:   0x88,
    }


class EOR(Opcode):
    opcodes = {
        Immediate: 0x49,
        Absolute:  0x4d,
        AbsoluteX: 0x5d,
        AbsoluteY: 0x59,
    }


class INC(Opcode):
    opcodes = {
        Absolute:  0xee,
        AbsoluteX: 0xfe,
    }


class INX(Opcode):
    opcodes = {
        Implied:   0xe8,
    }


class INY(Opcode):
    opcodes = {
        Implied:   0xc8,
    }


class JMP(Opcode):
    opcodes = {
        Absolute:  0x4c,
    }


class JSR(Opcode):
    opcodes = {
        Absolute:  0x20,
    }


class LDA(Opcode):
    opcodes = {
        Immediate: 0xa9,
        Absolute:  0xad,
        AbsoluteX: 0xbd,
        AbsoluteY: 0xb9,
    }


class LDX(Opcode):
    opcodes = {
        Immediate: 0xa2,
        Absolute:  0xae,
        AbsoluteY: 0xbe,
    }


class LDY(Opcode):
    opcodes = {
        Immediate: 0xa0,
        Absolute:  0xac,
        AbsoluteX: 0xbc,
    }


class ORA(Opcode):
    opcodes = {
        Immediate: 0x09,
        Absolute:  0x0d,
        AbsoluteX: 0x1d,
        AbsoluteY: 0x19,
    }


class ROL(Opcode):
    opcodes = {
        Implied:   0x2a,    # Accumulator
        Absolute:  0x2e,
        AbsoluteX: 0x3e,
    }


class ROR(Opcode):
    opcodes = {
        Implied:   0x6a,    # Accumulator
        Absolute:  0x6e,
        AbsoluteX: 0x7e,
    }


class RTS(Opcode):
    opcodes = {
        Implied:   0x60,
    }


class SBC(Opcode):
    opcodes = {
        Immediate: 0xe9,
        Absolute:  0xed,
        AbsoluteX: 0xfd,
        AbsoluteY: 0xf9,
    }


class SEC(Opcode):
    opcodes = {
        Implied:   0x38,
    }


class SEI(Opcode):
    opcodes = {
        Implied:   0x78,
    }


class STA(Opcode):
    opcodes = {
        Absolute:  0x8d,
        AbsoluteX: 0x9d,
        AbsoluteY: 0x99,
    }


class STX(Opcode):
    opcodes = {
        Absolute:  0x8e,
    }


class STY(Opcode):
    opcodes = {
        Absolute:  0x8c,
    }


class TAX(Opcode):
    opcodes = {
        Implied:   0xaa,
    }


class TAY(Opcode):
    opcodes = {
        Implied:   0xa8,
    }


class TXA(Opcode):
    opcodes = {
        Implied:   0x8a,
    }


class TYA(Opcode):
    opcodes = {
        Implied:   0x98,
    }
