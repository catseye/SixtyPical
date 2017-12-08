"""Data/storage model for SixtyPical."""

class Type(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'Type(%r)' % self.name

    def __eq__(self, other):
        return isinstance(other, Type) and other.name == self.name

    def __hash__(self):
        return hash(self.name)


TYPE_BIT = Type('bit')
TYPE_BYTE = Type('byte')
TYPE_BYTE_TABLE = Type('byte table')
TYPE_WORD = Type('word')
TYPE_WORD_TABLE = Type('word table')


class ExecutableType(Type):
    """Used for routines and vectors."""
    def __init__(self, name, inputs=None, outputs=None, trashes=None):
        self.name = name
        self.inputs = inputs or set()
        self.outputs = outputs or set()
        self.trashes = trashes or set()

    def __repr__(self):
        return 'RoutineType(%r, inputs=%r, outputs=%r, trashes=%r)' % (
            self.name, self.inputs, self.outputs, self.trashes
        )

    def __eq__(self, other):
        return isinstance(other, RoutineType) and (
            other.name == self.name and
            other.inputs == self.inputs and
            other.outputs == self.outputs and
            other.trashes == self.trashes
        )

    def __hash__(self):
        return hash(self.name) ^ hash(self.inputs) ^ hash(self.outputs) ^ hash(self.trashes)


class RoutineType(ExecutableType):
    """This memory location contains the code for a routine."""
    def __init__(self, **kwargs):
        super(RoutineType, self).__init__('routine', **kwargs)


class VectorType(ExecutableType):
    """This memory location contains the address of a routine."""
    def __init__(self, **kwargs):
        super(VectorType, self).__init__('vector', **kwargs)


class BufferType(Type):
    def __init__(self, size):
        self.size = size
        self.name = 'buffer[%s]' % self.size


class PointerType(Type):
    def __init__(self):
        self.name = 'pointer'


class Ref(object):
    def is_constant(self):
        """read-only means that the program cannot change the value
        of a location.  constant means that the value of the location
        will not change during the lifetime of the program.""" 
        raise NotImplementedError


class LocationRef(Ref):
    def __init__(self, type, name):
        self.type = type
        self.name = name

    def __eq__(self, other):
        # Ordinarily there will only be one ref with a given name,
        # but because we store the type in here and we want to treat
        # these objects as immutable, we compare the types, too.
        # Not sure if very wise.
        return isinstance(other, self.__class__) and (
            other.name == self.name and other.type == self.type
        )

    def __hash__(self):
        return hash(self.name + str(self.type))

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.type, self.name)

    def is_constant(self):
        return isinstance(self.type, RoutineType)


class IndirectRef(Ref):
    def __init__(self, ref):
        self.ref = ref

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.ref == other.ref

    def __hash__(self):
        return hash(self.__class__.name) ^ hash(self.ref)

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.ref)

    @property
    def name(self):
        return '[{}]+y'.format(self.ref.name)

    def is_constant(self):
        return False


class IndexedRef(Ref):
    def __init__(self, ref, index):
        self.ref = ref
        self.index = index

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.ref == other.ref and self.index == other.index

    def __hash__(self):
        return hash(self.__class__.name) ^ hash(self.ref) ^ hash(self.index)

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.ref, self.index)

    @property
    def name(self):
        return '{}+{}'.format(self.ref.name, self.index.name)

    def is_constant(self):
        return False


class AddressRef(Ref):
    def __init__(self, ref):
        self.ref = ref

    def __eq__(self, other):
        return self.ref == other.ref

    def __hash__(self):
        return hash(self.__class__.name) ^ hash(self.ref)

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.ref)

    @property
    def name(self):
        return '^{}'.format(self.ref.name)

    def is_constant(self):
        return True


class PartRef(Ref):
    """For 'low byte of' location and 'high byte of' location modifiers.

    height=0 = low byte, height=1 = high byte.

    NOTE: Not actually used yet.  Might require more thought before it's usable.
    """
    def __init__(self, ref, height):
        assert isinstance(ref, Ref)
        assert ref.type == TYPE_WORD
        self.ref = ref
        self.height = height
        self.type = TYPE_BYTE

    def __eq__(self, other):
        return isinstance(other, PartRef) and (
            other.height == self.height and other.ref == self.ref
        )

    def __hash__(self):
        return hash(self.ref) ^ hash(self.height) ^ hash(self.type)

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.ref, self.height)

    def is_constant(self):
        return self.ref.is_constant()


class ConstantRef(Ref):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __eq__(self, other):
        return isinstance(other, ConstantRef) and (
            other.type == self.type and other.value == self.value
        )

    def __hash__(self):
        return hash(str(self.value) + str(self.type))

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.type, self.value)

    def is_constant(self):
        return True

    def high_byte(self):
        return (self.value >> 8) & 255

    def low_byte(self):
        return self.value & 255


REG_A = LocationRef(TYPE_BYTE, 'a')
REG_X = LocationRef(TYPE_BYTE, 'x')
REG_Y = LocationRef(TYPE_BYTE, 'y')

FLAG_Z = LocationRef(TYPE_BIT, 'z')
FLAG_C = LocationRef(TYPE_BIT, 'c')
FLAG_N = LocationRef(TYPE_BIT, 'n')
FLAG_V = LocationRef(TYPE_BIT, 'v')
