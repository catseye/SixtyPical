"""Executable file writer."""

from sixtypical.emitter import Emitter, Byte, Word


class Outputter(object):
    def __init__(self, output_format):
        self.output_format = output_format
        if output_format == 'raw':
            self.start_addr = 0x0000
            self.prelude = []
        elif output_format == 'prg':
            self.start_addr = 0xc000
            self.prelude = []
        elif output_format == 'c64-basic-prg':
            self.start_addr = 0x0801
            self.prelude = [0x10, 0x08, 0xc9, 0x07, 0x9e, 0x32,
                            0x30, 0x36, 0x31, 0x00, 0x00, 0x00]
        elif output_format == 'vic20-basic-prg':
            self.start_addr = 0x1001
            self.prelude = [0x0b, 0x10, 0xc9, 0x07, 0x9e, 0x34,
                            0x31, 0x30, 0x39, 0x00, 0x00, 0x00]
        elif output_format == 'atari2600-cart':
            self.start_addr = 0xf000
            self.prelude = [0x78, 0xd8, 0xa2, 0xff, 0x9a, 0xa9,
                            0x00, 0x95, 0x00, 0xca, 0xd0, 0xfb]
        else:
            raise NotImplementedError("Unknown output format: {}".format(output_format))

    def set_start_addr(self, start_addr):
        self.start_addr = start_addr

    def write_prelude(self, fh):

        # If we are outputting a .PRG, we output the load address first.
        # We don't use the Emitter for this b/c not part of addr space.
        if self.output_format in ('prg', 'c64-basic-prg', 'vic20-basic-prg'):
            fh.write(bytearray(Word(self.start_addr).serialize(0)))

        emitter = Emitter(self.start_addr)
        for byte in self.prelude:
            emitter.emit(Byte(byte))

        return emitter

    def write_postlude(self, emitter):
        # If we are outputting a cartridge with boot and BRK address
        # at the end, pad to ROM size minus 4 bytes, and emit addresses.
        if self.output_format == 'atari2600-cart':
            emitter.pad_to_size(4096 - 4)
            emitter.emit(Word(self.start_addr))
            emitter.emit(Word(self.start_addr))
