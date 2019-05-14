# encoding: UTF-8

from sixtypical.model import (
    TYPE_BIT, TYPE_BYTE, LocationRef, 
)


class SymEntry(object):
    def __init__(self, ast_node, type_):
        self.ast_node = ast_node
        self.type_ = type_

    def __repr__(self):
        return "%s(%r, %r)" % (self.__class__.__name__, self.ast_node, self.type_)


class SymbolTable(object):
    def __init__(self):
        self.symbols = {}          # symbol name  -> SymEntry
        self.locals = {}           # routine name -> (symbol name -> SymEntry)
        self.typedefs = {}         # type name    -> Type AST
        self.consts = {}           # const name   -> ConstantRef

        for name in ('a', 'x', 'y'):
            self.symbols[name] = SymEntry(None, TYPE_BYTE)
        for name in ('c', 'z', 'n', 'v'):
            self.symbols[name] = SymEntry(None, TYPE_BIT)

    def __str__(self):
        return "Symbols: {}\nLocals: {}\nTypedefs: {}\nConsts: {}".format(self.symbols, self.locals, self.typedefs, self.consts)

    def has_local(self, routine_name, name):
        return name in self.locals.get(routine_name, {})

    def fetch_global_type(self, name):
        return self.symbols[name].type_

    def fetch_local_type(self, routine_name, name):
        return self.locals[routine_name][name].type_

    def fetch_global_ref(self, name):
        if name in self.symbols:
            return LocationRef(name)
        return None

    def fetch_local_ref(self, routine_name, name):
        routine_locals = self.locals.get(routine_name, {})
        if name in routine_locals:
            return LocationRef(name)
        return None
