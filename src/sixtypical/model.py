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


class Ref(object):
    pass


class LocationRef(Ref):
    def __init__(self, type, name):
        self.type = type
        self.name = name

    def __eq__(self, other):
        # Ordinarily there will only be one ref with a given name,
        # but because we store the type in here and we want to treat
        # these objects as immutable, we compare the types, too.
        # Not sure if very wise.
        return isinstance(other, LocationRef) and (
            other.name == self.name and other.type == self.type
        )

    def __hash__(self):
        return hash(self.name + str(self.type))

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.type, self.name)


class ConstantRef(Ref):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __repr__(self):
        return 'ConstantRef(%r)' % self.value

    def __eq__(self, other):
        return isinstance(other, ConstantRef) and (
            other.type == self.type and other.value == self.value
        )

    def __hash__(self):
        return hash(str(self.value) + str(self.type))

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.type, self.value)


REG_A = LocationRef(TYPE_BYTE, 'a')
REG_X = LocationRef(TYPE_BYTE, 'x')
REG_Y = LocationRef(TYPE_BYTE, 'y')

FLAG_Z = LocationRef(TYPE_BIT, 'z')
FLAG_C = LocationRef(TYPE_BIT, 'c')
FLAG_N = LocationRef(TYPE_BIT, 'n')
FLAG_V = LocationRef(TYPE_BIT, 'v')
