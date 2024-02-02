# Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
# This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
# SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical

"""Executable file writer."""

from sixtypical.emitter import Emitter, Byte, Word


class Outputter(object):
    def __init__(self, fh, start_addr=None):
        self.start_addr = self.__class__.start_addr
        if start_addr is not None:
            self.start_addr = start_addr
        self.prelude = self.__class__.prelude
        self.fh = fh
        self.emitter = Emitter(self.start_addr)

    def write_header(self):
        pass

    def write_prelude(self):
        self.write_header()
        for byte in self.prelude:
            self.emitter.emit(Byte(byte))

    def write_postlude(self):
        pass


class RawOutputter(Outputter):
    start_addr = 0x0000
    prelude = []


class PrgOutputter(Outputter):
    start_addr = 0xc000
    prelude = []

    def write_header(self):
        # If we are outputting a .PRG, we output the load address first.
        # We don't use the Emitter for this b/c not part of addr space.
        self.fh.write(bytearray(Word(self.start_addr).serialize(0)))


class C64BasicPrgOutputter(PrgOutputter):
    start_addr = 0x0801
    prelude = [0x10, 0x08, 0xc9, 0x07, 0x9e, 0x32,
               0x30, 0x36, 0x31, 0x00, 0x00, 0x00]


class Vic20BasicPrgOutputter(PrgOutputter):
    start_addr = 0x1001
    prelude = [0x0b, 0x10, 0xc9, 0x07, 0x9e, 0x34,
               0x31, 0x30, 0x39, 0x00, 0x00, 0x00]


class Atari2600CartOutputter(Outputter):
    start_addr = 0xf000
    prelude = [0x78, 0xd8, 0xa2, 0xff, 0x9a, 0xa9,
               0x00, 0x95, 0x00, 0xca, 0xd0, 0xfb]

    def write_postlude(self):
        # If we are outputting a cartridge with boot and BRK address
        # at the end, pad to ROM size minus 4 bytes, and emit addresses.
        self.emitter.pad_to_size(4096 - 4)
        self.emitter.emit(Word(self.start_addr))
        self.emitter.emit(Word(self.start_addr))


def outputter_class_for(output_format):
    return {
        'raw': RawOutputter,
        'prg': PrgOutputter,
        'c64-basic-prg': C64BasicPrgOutputter,
        'vic20-basic-prg': Vic20BasicPrgOutputter,
        'atari2600-cart': Atari2600CartOutputter,
    }[output_format]
