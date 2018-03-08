"""Data/storage model for SixtyPical."""


class Type(object):
    def __init__(self, name, max_range=None):
        self.name = name
        self.max_range = max_range

    def __repr__(self):
        return 'Type(%r)' % self.name

    def __str__(self):
        return self.name

    def __eq__(self, other):
        return isinstance(other, Type) and other.name == self.name

    def __hash__(self):
        return hash(self.name)

    def backpatch_constraint_labels(self, resolver):
        def resolve(w):
             if not isinstance(w, basestring):
                 return w
             return resolver(w)
        if isinstance(self, TableType):
            self.of_type.backpatch_constraint_labels(resolver)
        elif isinstance(self, VectorType):
            self.of_type.backpatch_constraint_labels(resolver)
        elif isinstance(self, RoutineType):
            self.inputs = set([resolve(w) for w in self.inputs])
            self.outputs = set([resolve(w) for w in self.outputs])
            self.trashes = set([resolve(w) for w in self.trashes])


TYPE_BIT = Type('bit', max_range=(0, 1))
TYPE_BYTE = Type('byte', max_range=(0, 255))
TYPE_WORD = Type('word', max_range=(0, 65535))



class RoutineType(Type):
    """This memory location contains the code for a routine."""
    def __init__(self, inputs=None, outputs=None, trashes=None):
        self.name = 'routine'
        self.inputs = inputs or set()
        self.outputs = outputs or set()
        self.trashes = trashes or set()

    def __repr__(self):
        return '%s(%r, inputs=%r, outputs=%r, trashes=%r)' % (
            self.__class__.__name__, self.name, self.inputs, self.outputs, self.trashes
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
        self.name = 'vector'
        self.of_type = of_type

    def __repr__(self):
        return '%s(%r)' % (
            self.__class__.__name__, self.of_type
        )

    def __eq__(self, other):
        return self.name == other.name and self.of_type == other.of_type

    def __hash__(self):
        return hash(self.name) ^ hash(self.of_type)


class TableType(Type):
    def __init__(self, of_type, size):
        self.of_type = of_type
        self.size = size
        self.name = '{} table[{}]'.format(self.of_type.name, self.size)

    def __repr__(self):
        return '%s(%r, %r)' % (
            self.__class__.__name__, self.of_type, self.size
        )

    @classmethod
    def is_a_table_type(cls_, x, of_type):
        return isinstance(x, TableType) and x.of_type == of_type


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
        return hash(self.name + str(self.type))

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
        return '{%s}' % ', '.join([str(loc) for loc in sorted(location_refs)])


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

    def max_range(self):
        return (self.value, self.value)

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
