class Emittable(object):
    def size(self):
        """Default implementation may not be very efficient."""
        return len(self.serialize())

    def serialize(self):
        raise NotImplementedError


class Byte(Emittable):
    def __init__(self, value):
        if value < -127 or value > 255:
            raise IndexError(thing)
        if value < 0:
            value += 256
        self.value = value

    def size(self):
        return 1

    def serialize(self):
        return chr(self.value)

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.value)


class Word(Emittable):
    def __init__(self, value):
        # TODO: range-checking
        self.value = value

    def size(self):
        return 2

    def serialize(self):
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

    def serialize(self):
        assert self.addr is not None, "unresolved label: %s" % self.name
        return Word(self.addr).serialize()


class Emitter(object):
    def __init__(self, addr):
        self.accum = []
        self.addr = addr
        self.name_counter = 0

    def emit(self, *things):
        for thing in things:
            if isinstance(thing, int):
                thing = Byte(thing)
            self.accum.append(thing)
            self.addr += thing.size()

    def serialize(self, stream):
        for emittable in self.accum:
            stream.write(emittable.serialize())

    def make_label(self, name=None):
        if name is None:
            name = 'label' + str(self.name_counter)
            self.name_counter += 1
        return Label(name, addr=self.addr)

    def resolve_label(self, label):
        label.set_addr(self.addr)
