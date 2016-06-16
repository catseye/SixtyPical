# encoding: UTF-8

class AST(object):
    def __init__(self, **kwargs):
        self.attrs = kwargs

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.attrs)

    def __getattr__(self, name):
        if name in self.attrs:
            return self.attrs[name]
        raise AttributeError(name)


class Program(AST):
    pass


class Defn(AST):
    pass


class Routine(AST):
    pass


class Block(AST):
    pass


class Instr(AST):
    pass
