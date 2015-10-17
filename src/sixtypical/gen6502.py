"""This is just a sketch for now."""

from sixtypical.emitter import Emitter, Word, Label


class Generator(object):
    def __init__(self, emitter):
        self.emitter = emitter

    ### ld ###

    def gen_lda_imm(self, b):
        self.emitter.emit(0xa9, b)

    def gen_lda_abs(self, addr):
        self.emitter.emit(0xad, addr)

    def gen_ldx_imm(self, b):
        self.emitter.emit(0xa2, b)

    def gen_ldx_abs(self, addr):
        self.emitter.emit(0xae, addr)

    def gen_tax(self):
        self.emitter.emit(0xaa)

    def gen_tay(self):
        self.emitter.emit(0xa8)

    def gen_txa(self):
        self.emitter.emit(0x8a)

    def gen_tya(self):
        self.emitter.emit(0x98)

    ### st ###

    def gen_sta_abs(self, addr):
        self.emitter.emit(0x8d, addr)

    def gen_stx_abs(self, addr):
        self.emitter.emit(0x8e, addr)

    def gen_sty_abs(self, addr):
        self.emitter.emit(0x8c, addr)

    ### add ###

    def gen_adc_imm(self, b):
        self.emitter.emit(0x69, b)

    def gen_adc_abs(self, addr):
        self.emitter.emit(0x6d, addr)

    ### sub ###

    def gen_sbc_imm(self, b):
        self.emitter.emit(0xe9, b)

    def gen_sbc_abs(self, addr):
        self.emitter.emit(0xed, addr)

    ### inc ###

    def gen_inc_abs(self, addr):
        self.emitter.emit(0xee, addr)

    def gen_inx(self):
        self.emitter.emit(0xe8)

    def gen_iny(self):
        self.emitter.emit(0xc8)

    ### dec ###

    def gen_dec_abs(self, addr):
        self.emitter.emit(0xce, addr)

    def gen_dex(self):
        self.emitter.emit(0xca)

    def gen_dey(self):
        self.emitter.emit(0x88)

    ### and ###

    def gen_and_imm(self, b):
        self.emitter.emit(0x29, b)

    def gen_and_abs(self, addr):
        self.emitter.emit(0x2d, addr)
