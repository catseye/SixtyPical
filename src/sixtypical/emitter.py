# Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
# This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
# SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical

"""Binary machine code emitter.  Used in SixtyPical to emit 6502 machine code,
but not specific to SixtyPical, or 6502.  Not even necessarily machine code -
though some parts are written around the assumptions of 8-bit architectures."""


class Emittable(object):
    def size(self):
        raise NotImplementedError

    def serialize(self, addr):
        """Should return an array of unsigned bytes (integers from 0 to 255.)
        `addr` is the address the value is being serialized at; for most objects
        it makes no difference, but some objects (like relative branches) do care."""
        raise NotImplementedError


class Byte(Emittable):
    def __init__(self, value):
        if isinstance(value, str):
            value = ord(value)
        if value < -127 or value > 255:
            raise IndexError(value)
        if value < 0:
            value += 256
        self.value = value

    def size(self):
        return 1

    def serialize(self, addr):
        return [self.value]

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.value)


class Word(Emittable):
    def __init__(self, value):
        # TODO: range-checking
        self.value = value

    def size(self):
        return 2

    def serialize(self, addr):
        word = self.value
        low = word & 255
        high = (word >> 8) & 255
        return [low, high]

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.value)


class Table(Emittable):
    def __init__(self, value, size):
        """`value` should be an iterable of Emittables."""
        # TODO: range-checking
        self.value = value
        self._size = size

    def size(self):
        return self._size

    def serialize(self, addr):
        buf = []
        for emittable in self.value:
            buf.extend(emittable.serialize(addr))  # FIXME: addr + offset
        while len(buf) < self.size():
            buf.append(0)
        return buf

    def __repr__(self):
        return "%s()" % (self.__class__.__name__)


class Label(Emittable):
    def __init__(self, name, addr=None, length=None):
        self.name = name
        self.addr = addr
        self.length = length

    def set_addr(self, addr):
        self.addr = addr

    def set_length(self, length):
        self.length = length

    def size(self):
        return 2

    def serialize(self, addr, offset=0):
        assert self.addr is not None, "unresolved label: %s" % self.name
        return Word(self.addr + offset).serialize(addr)

    def serialize_relative_to(self, addr):
        assert self.addr is not None, "unresolved label: %s" % self.name
        return Byte(self.addr - (addr + 2)).serialize(addr)

    def serialize_as_zero_page(self, addr, offset=0):
        assert self.addr is not None, "unresolved label: %s" % self.name
        return Byte(self.addr + offset).serialize(addr)

    def __repr__(self):
        addr_s = ', addr=%r' % self.addr if self.addr is not None else ''
        length_s = ', length=%r' % self.length if self.length is not None else ''
        return "%s(%r%s%s)" % (self.__class__.__name__, self.name, addr_s, length_s)


class Offset(Emittable):
    def __init__(self, label, offset):
        assert isinstance(label, Label)
        self.label = label
        self.offset = offset

    def size(self):
        self.label.size()

    def serialize(self, addr):
        return self.label.serialize(addr, offset=self.offset)

    def serialize_as_zero_page(self, addr, offset=0):
        return self.label.serialize_as_zero_page(addr, offset=self.offset)

    def __repr__(self):
        return "%s(%r, %r)" % (self.__class__.__name__, self.label, self.offset)


class HighAddressByte(Emittable):
    def __init__(self, label):
        assert isinstance(label, (Label, Offset))
        self.label = label

    def size(self):
        return 1

    def serialize(self, addr):
        return [self.label.serialize(addr)[0]]

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.label)


class LowAddressByte(Emittable):
    def __init__(self, label):
        assert isinstance(label, (Label, Offset))
        self.label = label

    def size(self):
        return 1

    def serialize(self, addr):
        return [self.label.serialize(addr)[1]]

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.label)


# - - - -


class Emitter(object):
    def __init__(self, addr):
        self.accum = []
        self.start_addr = addr
        self.addr = addr
        self.name_counter = 0

    def emit(self, *things):
        for thing in things:
            self.accum.append(thing)
            self.addr += thing.size()

    def get_tail(self):
        if self.accum:
            return self.accum[-1]
        else:
            return None

    def retract(self):
        thing = self.accum.pop()
        self.addr -= thing.size()

    def serialize_to(self, stream):
        """`stream` should be a file opened in binary mode."""
        addr = self.start_addr
        for emittable in self.accum:
            chunk = emittable.serialize(addr)
            stream.write(bytearray(chunk))
            addr += len(chunk)

    def make_label(self, name=None):
        if name is None:
            name = 'label' + str(self.name_counter)
            self.name_counter += 1
        return Label(name, addr=self.addr)

    def resolve_label(self, label):
        label.set_addr(self.addr)

    def resolve_bss_label(self, label):
        """Set the given label to be at the current address and
        advance the address for the next label, but don't emit anything."""
        self.resolve_label(label)
        self.addr += label.length

    def size(self):
        return sum(emittable.size() for emittable in self.accum)

    def pad_to_size(self, size):
        self_size = self.size()
        if self_size > size:
            raise IndexError("Emitter size {} exceeds pad size {}".format(self_size, size))
        num_bytes = size - self_size
        if num_bytes > 0:
            self.accum.extend([Byte(0)] * num_bytes)
