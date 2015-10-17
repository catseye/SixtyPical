class Word(object):
    def __init__(self, value):
        self.value = value

    def size(self):
        return 2

    def serialize(self):
        word = self.value
        low = word & 255
        high = (word >> 8) & 255
        return chr(low) + chr(high)


class Label(object):
    def __init__(self, name, addr=None):
        self.name = name
        self.addr = addr

    def set_addr(self, addr):
        self.addr = addr

    def size(self):
        return 2

    def serialize(self):
        if self.addr is None:
            raise ValueError(self.addr)
        return Word(self.addr).serialize()


class Emitter(object):
    def __init__(self, addr):
        self.accum = []
        self.addr = addr
        self.name_counter = 0

    def gen(self, *things):
        for thing in things:
            if isinstance(thing, int):
                if thing < -127 or thing > 255:
                    raise ValueError(thing)
                if thing < 0:
                    thing += 256
                self.accum.append(thing)
                self.addr += 1
            else:
                self.accum.append(thing)
                self.addr += thing.size()

    def serialize(self, stream):
        for thing in self.accum:
            if isintance(thing, int):
                stream.write(chr(thing))
            else:
                stream.write(thing.serialize())

    def make_label(self, name=None):
        if name is None:
            name = 'label' + str(self.name_counter)
            self.name_counter += 1
        return Label(name, addr=self.addr)

    def resolve_label(self, label):
        label.set_addr(self.addr)
