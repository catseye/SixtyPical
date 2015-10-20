class Emittable(object):
    def size(self):
        raise NotImplementedError

    def serialize(self, addr):
        raise NotImplementedError


class Byte(Emittable):
    def __init__(self, value):
        if value < -127 or value > 255:
            raise IndexError(value)
        if value < 0:
            value += 256
        self.value = value

    def size(self):
        return 1

    def serialize(self, addr=None):
        return chr(self.value)

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.value)


class Word(Emittable):
    def __init__(self, value):
        # TODO: range-checking
        self.value = value

    def size(self):
        return 2

    def serialize(self, addr=None):
        word = self.value
        low = word & 255
        high = (word >> 8) & 255
        return chr(low) + chr(high)

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.value)


class Label(Emittable):
    def __init__(self, name, addr=None):
        self.name = name
        self.addr = addr

    def set_addr(self, addr):
        self.addr = addr

    def size(self):
        return 2

    def serialize(self, addr=None, offset=0):
        assert self.addr is not None, "unresolved label: %s" % self.name
        return Word(self.addr + offset).serialize()

    def serialize_relative_to(self, addr):
        assert self.addr is not None, "unresolved label: %s" % self.name
        return Byte(self.addr - (addr + 2)).serialize()

    def __repr__(self):
        addrs = ', addr=%r' % self.addr if self.addr is not None else ''
        return "%s(%r%s)" % (self.__class__.__name__, self.name, addrs)


class Offset(Emittable):
    def __init__(self, label, offset):
        assert isinstance(label, Label)
        self.label = label
        self.offset = offset

    def size(self):
        self.label.size()

    def serialize(self, addr=None):
        return self.label.serialize(offset=self.offset)

    def __repr__(self):
        return "%s(%r, %r)" % (self.__class__.__name__, self.label, self.offset)


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

    def serialize(self, stream):
        addr = self.start_addr
        for emittable in self.accum:
            chunk = emittable.serialize(addr)
            stream.write(chunk)
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
        self.addr += label.size()
