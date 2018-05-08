# encoding: UTF-8

class AST(object):
    children_attrs = ()
    child_attrs = ()
    value_attrs = ()

    def __init__(self, line_number, **kwargs):
        self.line_number = line_number
        self.attrs = {}
        for attr in self.children_attrs:
            self.attrs[attr] = kwargs.pop(attr, [])
            for child in self.attrs[attr]:
                assert child is None or isinstance(child, AST), \
                  "child %s=%r of %r is not an AST node" % (attr, child, self)
        for attr in self.child_attrs:
            self.attrs[attr] = kwargs.pop(attr, None)
            child = self.attrs[attr]
            assert child is None or isinstance(child, AST), \
              "child %s=%r of %r is not an AST node" % (attr, child, self)
        for attr in self.value_attrs:
            self.attrs[attr] = kwargs.pop(attr, None)
        assert (not kwargs), "extra arguments supplied to {} node: {}".format(self.type, kwargs)

    @property
    def type(self):
        return self.__class__.__name__

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.attrs)

    def __getattr__(self, name):
        if name in self.attrs:
            return self.attrs[name]
        raise AttributeError(name)

    def all_children(self):
        for attr in self.children_attrs:
            for child in self.attrs[attr]:
                yield child
                for subchild in child.all_children():
                    yield subchild
        for attr in self.child_attrs:
            child = self.attrs[attr]
            yield child
            for subchild in child.all_children():
                yield subchild


class Program(AST):
    children_attrs = ('defns', 'routines',)


class Defn(AST):
    value_attrs = ('name', 'addr', 'initial', 'location',)


class Routine(AST):
    value_attrs = ('name', 'addr', 'initial', 'location',)
    children_attrs = ('statics',)
    child_attrs = ('block',)


class Block(AST):
    children_attrs = ('instrs',)


class Instr(AST):
    pass


class SingleOp(Instr):
    value_attrs = ('opcode', 'dest', 'src', 'location',)


class If(Instr):
    value_attrs = ('src', 'inverted',)
    child_attrs = ('block1', 'block2',)


class Repeat(Instr):
    value_attrs = ('src', 'inverted',)
    child_attrs = ('block',)


class For(Instr):
    value_attrs = ('dest', 'direction', 'final',)
    child_attrs = ('block',)


class WithInterruptsOff(Instr):
    child_attrs = ('block',)


class Save(Instr):
    value_attrs = ('locations',)
    child_attrs = ('block',)
