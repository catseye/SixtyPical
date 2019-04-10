"""Data/storage model for SixtyPical."""

from collections import namedtuple


class BitType(namedtuple('BitType', ['typename'])):
    max_range = (0, 1)
    def __new__(cls):
        return super(BitType, cls).__new__(cls, 'bit')
TYPE_BIT = BitType()


class ByteType(namedtuple('ByteType', ['typename'])):
    max_range = (0, 255)
    def __new__(cls):
        return super(ByteType, cls).__new__(cls, 'byte')
TYPE_BYTE = ByteType()


class WordType(namedtuple('WordType', ['typename'])):
    max_range = (0, 65535)
    def __new__(cls):
        return super(WordType, cls).__new__(cls, 'word')
TYPE_WORD = WordType()


class RoutineType(namedtuple('RoutineType', ['typename', 'inputs', 'outputs', 'trashes'])):
    """This memory location contains the code for a routine."""
    max_range = (0, 0)

    def __new__(cls, *args):
        return super(RoutineType, cls).__new__(cls, 'routine', *args)

    @classmethod
    def executable_types_compatible(cls_, src, dest):
        """Returns True iff a value of type `src` can be assigned to a storage location of type `dest`."""
        if isinstance(src, VectorType):
            src = src.of_type
        if isinstance(dest, VectorType):
            dest = dest.of_type
        if isinstance(src, RoutineType) and isinstance(dest, RoutineType):
            # TODO: I'm sure we can replace some of these with subset-containment, but that requires thought
            return (
                src.inputs == dest.inputs and
                src.outputs == dest.outputs and
                src.trashes == dest.trashes
            )
        else:
            return False


class VectorType(namedtuple('VectorType', ['typename', 'of_type'])):
    """This memory location contains the address of some other type (currently, only RoutineType)."""
    max_range = (0, 65535)

    def __new__(cls, *args):
        return super(VectorType, cls).__new__(cls, 'vector', *args)


class TableType(namedtuple('TableType', ['typename', 'of_type', 'size'])):

    def __new__(cls, *args):
        return super(TableType, cls).__new__(cls, 'table', *args)

    @property
    def max_range(self):
        return (0, self.size - 1)

    @classmethod
    def is_a_table_type(cls_, x, of_type):
        return isinstance(x, TableType) and x.of_type == of_type


class PointerType(namedtuple('PointerType', ['typename'])):
    max_range = (0, 65535)

    def __new__(cls):
        return super(PointerType, cls).__new__(cls, 'pointer')


# --------------------------------------------------------


class LocationRef(namedtuple('LocationRef', ['reftype', 'name'])):
    def __new__(cls, *args):
        return super(LocationRef, cls).__new__(cls, 'location', *args)

    @classmethod
    def format_set(cls, location_refs):
        return '{%s}' % ', '.join([str(loc) for loc in sorted(location_refs, key=lambda x: x.name)])


class IndirectRef(namedtuple('IndirectRef', ['reftype', 'ref'])):
    def __new__(cls, *args):
        return super(IndirectRef, cls).__new__(cls, 'indirect', *args)

    @property
    def name(self):
        return '[{}]+y'.format(self.ref.name)


class IndexedRef(namedtuple('IndexedRef', ['reftype', 'ref', 'offset', 'index'])):
    def __new__(cls, *args):
        return super(IndexedRef, cls).__new__(cls, 'indexed', *args)

    @property
    def name(self):
        return '{}+{}+{}'.format(self.ref.name, self.offset, self.index.name)


class ConstantRef(namedtuple('ConstantRef', ['reftype', 'type', 'value'])):
    def __new__(cls, *args):
        return super(ConstantRef, cls).__new__(cls, 'constant', *args)

    def high_byte(self):
        return (self.value >> 8) & 255

    def low_byte(self):
        return self.value & 255

    def pred(self):
        assert self.type == TYPE_BYTE
        value = self.value - 1
        while value < 0:
            value += 256
        return ConstantRef(self.type, value)

    def succ(self):
        assert self.type == TYPE_BYTE
        value = self.value + 1
        while value > 255:
            value -= 256
        return ConstantRef(self.type, value)

    @property
    def name(self):
        return 'constant({})'.format(self.value)


REG_A = LocationRef('a')
REG_X = LocationRef('x')
REG_Y = LocationRef('y')

FLAG_Z = LocationRef('z')
FLAG_C = LocationRef('c')
FLAG_N = LocationRef('n')
FLAG_V = LocationRef('v')
