

class Type(object):
    pass

class BitType(Type):
    pass

class ByteType(Type):
    pass

class MemoryLocation(object):
    def __init__(self, name, type_=ByteType, readonly=False):
        self.name = name
        self.type_ = type_
        self.readonly = readonly


regA = MemoryLocation('a')
regX = MemoryLocation('x')
regY = MemoryLocation('y')

regC = MemoryLocation('c', type=BitType)
regZ = MemoryLocation('z', type=BitType)
regN = MemoryLocation('n', type=BitType)
regV = MemoryLocation('v', type=BitType)


class Context(dict):
    # maps MemoryLoction -> properties: uninitialized, initialized, written
    pass
