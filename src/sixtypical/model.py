"""Data/storage model for SixtyPical."""


class Type(object):
    def __init__(self, name, max_range=None):
        self.name = name
        self.max_range = max_range

    def __repr__(self):
        return 'Type(%r)' % self.name

    def __eq__(self, other):
        return other.__class__ == self.__class__ and other.name == self.name


TYPE_BIT = Type('bit', max_range=(0, 1))
TYPE_BYTE = Type('byte', max_range=(0, 255))
TYPE_WORD = Type('word', max_range=(0, 65535))


class RoutineType(Type):
    """This memory location contains the code for a routine."""
    def __init__(self, inputs, outputs, trashes):
        self.inputs = inputs
        self.outputs = outputs
        self.trashes = trashes

    def __repr__(self):
        return '%s(inputs=%r, outputs=%r, trashes=%r)' % (
            self.__class__.__name__, self.inputs, self.outputs, self.trashes
        )

    def __eq__(self, other):
        return isinstance(other, RoutineType) and (
            other.inputs == self.inputs and
            other.outputs == self.outputs and
            other.trashes == self.trashes
        )

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


class VectorType(Type):
    """This memory location contains the address of some other type (currently, only RoutineType)."""
    def __init__(self, of_type):
        self.of_type = of_type

    def __repr__(self):
        return '%s(%r)' % (
            self.__class__.__name__, self.of_type
        )

    def __eq__(self, other):
        return isinstance(other, VectorType) and self.of_type == other.of_type


class TableType(Type):
    def __init__(self, of_type, size):
        self.of_type = of_type
        self.size = size

    def __repr__(self):
        return '%s(%r, %r)' % (
            self.__class__.__name__, self.of_type, self.size
        )

    def __eq__(self, other):
        return isinstance(other, TableType) and self.of_type == other.of_type and self.size == other.size

    @property
    def max_range(self):
        return (0, self.size - 1)

    @classmethod
    def is_a_table_type(cls_, x, of_type):
        return isinstance(x, TableType) and x.of_type == of_type


class PointerType(Type):
    def __init__(self):
        self.name = 'pointer'

    def __eq__(self, other):
        return other.__class__ == self.__class__


class Ref(object):
    def is_constant(self):
        """read-only means that the program cannot change the value
        of a location.  constant means that the value of the location
        will not change during the lifetime of the program.""" 
        raise NotImplementedError("class {} must implement is_constant()".format(self.__class__.__name__))

    def max_range(self):
        raise NotImplementedError("class {} must implement max_range()".format(self.__class__.__name__))


class LocationRef(Ref):
    def __init__(self, type, name):
        self.type = type
        self.name = name

    def __eq__(self, other):
        # Ordinarily there will only be one ref with a given name,
        # but because we store the type in here and we want to treat
        # these objects as immutable, we compare the types, too,
        # just to be sure.
        equal = isinstance(other, self.__class__) and other.name == self.name
        if equal:
            assert other.type == self.type, repr((self, other))
        return equal

    def __hash__(self):
        return hash(self.name + repr(self.type))

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.type, self.name)

    def __str__(self):
        return "{}:{}".format(self.name, self.type)

    def is_constant(self):
        return isinstance(self.type, RoutineType)

    def max_range(self):
        try:
            return self.type.max_range
        except:
            return (0, 0)

    @classmethod
    def format_set(cls, location_refs):
        return '{%s}' % ', '.join([str(loc) for loc in sorted(location_refs, key=lambda x: x.name)])


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
    def __init__(self, ref, offset, index):
        self.ref = ref
        self.offset = offset
        self.index = index

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.ref == other.ref and self.offset == other.offset and self.index == other.index

    def __hash__(self):
        return hash(self.__class__.name) ^ hash(self.ref) ^ hash(self.offset) ^ hash(self.index)

    def __repr__(self):
        return '%s(%r, %r, %r)' % (self.__class__.__name__, self.ref, self.offset, self.index)

    @property
    def name(self):
        return '{}+{}+{}'.format(self.ref.name, self.offset, self.index.name)

    def is_constant(self):
        return False


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

    def max_range(self):
        return (self.value, self.value)

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


REG_A = LocationRef(TYPE_BYTE, 'a')
REG_X = LocationRef(TYPE_BYTE, 'x')
REG_Y = LocationRef(TYPE_BYTE, 'y')

FLAG_Z = LocationRef(TYPE_BIT, 'z')
FLAG_C = LocationRef(TYPE_BIT, 'c')
FLAG_N = LocationRef(TYPE_BIT, 'n')
FLAG_V = LocationRef(TYPE_BIT, 'v')
