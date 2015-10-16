"""Data/storage model for SixtyPical."""

class LocationRef(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'LocationRef(%r)' % self.name


class ConstantRef(object):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return 'ConstantRef(%r)' % self.value


# TODO type=byte

REG_A = LocationRef('a')
REG_X = LocationRef('x')
REG_Y = LocationRef('y')

# TODO type=bit

FLAG_Z = LocationRef('z')
FLAG_C = LocationRef('c')
FLAG_N = LocationRef('n')
FLAG_V = LocationRef('v')
