# encoding: UTF-8

from sixtypical.ast import Program, Defn, Routine, Block, SingleOp, BlockOp, IfOp
from sixtypical.model import (
    TYPE_BIT, TYPE_BYTE, TYPE_WORD,
    RoutineType, VectorType, TableType, BufferType, PointerType,
    LocationRef, ConstantRef, IndirectRef, IndexedRef, AddressRef,
)
from sixtypical.scanner import Scanner


class SymEntry(object):
    def __init__(self, ast_node, model):
        self.ast_node = ast_node
        self.model = model

    def __repr__(self):
        return "%s(%r, %r)" % (self.__class__.__name__, self.ast_node, self.model)


class Parser(object):
    def __init__(self, text):
        self.scanner = Scanner(text)
        self.symbols = {}          # token -> SymEntry
        self.current_statics = {}  # token -> SymEntry
        self.typedefs = {}         # token -> Type AST
        for token in ('a', 'x', 'y'):
            self.symbols[token] = SymEntry(None, LocationRef(TYPE_BYTE, token))
        for token in ('c', 'z', 'n', 'v'):
            self.symbols[token] = SymEntry(None, LocationRef(TYPE_BIT, token))
        self.backpatch_instrs = []

    def soft_lookup(self, name):
        if name in self.current_statics:
            return self.current_statics[name].model
        if name in self.symbols:
            return self.symbols[name].model
        return None

    def lookup(self, name):
        model = self.soft_lookup(name)
        if model is None:
            raise SyntaxError('Undefined symbol "%s"' % name)
        return model

    # --- grammar productions

    def program(self):
        defns = []
        routines = []
        while self.scanner.on('typedef'):
            typedef = self.typedef()
        typenames = ['byte', 'word', 'table', 'vector', 'buffer', 'pointer']  # 'routine',
        typenames.extend(self.typedefs.keys())
        while self.scanner.on(*typenames):
            defn = self.defn()
            name = defn.name
            if name in self.symbols:
                raise SyntaxError('Symbol "%s" already declared' % name)
            self.symbols[name] = SymEntry(defn, defn.location)
            defns.append(defn)
        while self.scanner.on('define', 'routine'):
            if self.scanner.consume('define'):
                name = self.scanner.token
                self.scanner.scan()
                routine = self.routine(name)
            else:
                routine = self.legacy_routine()
                name = routine.name
            if name in self.symbols:
                raise SyntaxError('Symbol "%s" already declared' % name)
            self.symbols[name] = SymEntry(routine, routine.location)
            routines.append(routine)
        self.scanner.check_type('EOF')

        # now backpatch the executable types.
        #for type_name, type_ in self.typedefs.iteritems():
        #    type_.backpatch_constraint_labels(lambda w: self.lookup(w))
        for defn in defns:
            defn.location.type.backpatch_constraint_labels(lambda w: self.lookup(w))
        for routine in routines:
            routine.location.type.backpatch_constraint_labels(lambda w: self.lookup(w))
        for instr in self.backpatch_instrs:
            if instr.opcode in ('call', 'goto'):
                name = instr.location
                if name not in self.symbols:
                    raise SyntaxError('Undefined routine "%s"' % name)
                if not isinstance(self.symbols[name].model.type, (RoutineType, VectorType)):
                    raise SyntaxError('Illegal call of non-executable "%s"' % name)
                instr.location = self.symbols[name].model
            if instr.opcode in ('copy',) and isinstance(instr.src, basestring):
                name = instr.src
                if name not in self.symbols:
                    raise SyntaxError('Undefined routine "%s"' % name)
                if not isinstance(self.symbols[name].model.type, (RoutineType, VectorType)):
                    raise SyntaxError('Illegal copy of non-executable "%s"' % name)
                instr.src = self.symbols[name].model

        return Program(defns=defns, routines=routines)

    def typedef(self):
        self.scanner.expect('typedef')
        type_ = self.defn_type()
        name = self.defn_name()
        if name in self.typedefs:
            raise SyntaxError('Type "%s" already declared' % name)
        self.typedefs[name] = type_
        return type_

    def defn(self):
        type_ = self.defn_type()
        name = self.defn_name()

        initial = None
        if self.scanner.consume(':'):
            if isinstance(type_, TableType) and self.scanner.on_type('string literal'):
                initial = self.scanner.token
            else:
                self.scanner.check_type('integer literal')
                initial = int(self.scanner.token)
            self.scanner.scan()

        addr = None
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            addr = int(self.scanner.token)
            self.scanner.scan()

        if initial is not None and addr is not None:
            raise SyntaxError("Definition cannot have both initial value and explicit address")

        location = LocationRef(type_, name)

        return Defn(name=name, addr=addr, initial=initial, location=location)

    def defn_size(self):
        self.scanner.expect('[')
        self.scanner.check_type('integer literal')
        size = int(self.scanner.token)
        self.scanner.scan()
        self.scanner.expect(']')
        return size

    def defn_type(self):
        type_ = self.defn_type_term()

        if self.scanner.consume('table'):
            size = self.defn_size()
            if size <= 0 or size > 256:
                raise SyntaxError("Table size must be > 0 and <= 256")
            type_ = TableType(type_, size)

        return type_

    def defn_type_term(self):
        type_ = None

        if self.scanner.consume('('):
            type_ = self.defn_type()
            self.scanner.expect(')')
            return type_

        if self.scanner.consume('byte'):
            type_ = TYPE_BYTE
        elif self.scanner.consume('word'):
            type_ = TYPE_WORD
        elif self.scanner.consume('vector'):
            type_ = self.defn_type_term()
            if not isinstance(type_, RoutineType):
                raise SyntaxError("Vectors can only be of a routine, not %r" % type_)
            type_ = VectorType(type_)
        elif self.scanner.consume('routine'):
            (inputs, outputs, trashes) = self.constraints()
            type_ = RoutineType(inputs=inputs, outputs=outputs, trashes=trashes)
        elif self.scanner.consume('buffer'):
            size = self.defn_size()
            type_ = BufferType(size)
        elif self.scanner.consume('pointer'):
            type_ = PointerType()
        else:
            type_name = self.scanner.token
            self.scanner.scan()
            if type_name not in self.typedefs:
                raise SyntaxError("Undefined type '%s'" % type_name)
            type_ = self.typedefs[type_name]

        return type_

    def defn_name(self):
        self.scanner.check_type('identifier')
        name = self.scanner.token
        self.scanner.scan()
        return name

    def constraints(self):
        inputs = set()
        outputs = set()
        trashes = set()
        if self.scanner.consume('inputs'):
            inputs = set(self.labels())
        if self.scanner.consume('outputs'):
            outputs = set(self.labels())
        if self.scanner.consume('trashes'):
            trashes = set(self.labels())
        return (inputs, outputs, trashes)

    def legacy_routine(self):
        self.scanner.expect('routine')
        name = self.scanner.token
        self.scanner.scan()
        (inputs, outputs, trashes) = self.constraints()
        type_ = RoutineType(inputs=inputs, outputs=outputs, trashes=trashes)
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            block = None
            addr = int(self.scanner.token)
            self.scanner.scan()
        else:
            block = self.block()
            addr = None
        location = LocationRef(type_, name)
        return Routine(
            name=name, block=block, addr=addr,
            location=location
        )

    def routine(self, name):
        type_ = self.defn_type()
        if not isinstance(type_, RoutineType):
            raise SyntaxError("Can only define a routine, not %r" % type_)
        statics = []
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            block = None
            addr = int(self.scanner.token)
            self.scanner.scan()
        else:
            statics = self.statics()

            self.current_statics = self.compose_statics_dict(statics)
            block = self.block()
            self.current_statics = {}

            addr = None
        location = LocationRef(type_, name)
        return Routine(
            name=name, block=block, addr=addr,
            location=location, statics=statics
        )

    def compose_statics_dict(self, statics):
        c = {}
        for defn in statics:
            name = defn.name
            if name in self.symbols or name in self.current_statics:
                raise SyntaxError('Symbol "%s" already declared' % name)
            c[name] = SymEntry(defn, defn.location)
        return c

    def labels(self):
        accum = []
        accum.append(self.label())
        while self.scanner.consume(','):
            accum.append(self.label())
        return accum

    def label(self):
        """Like a locexpr, but does not allow literal values, and the labels do not
        need to be defined yet.  They will be resolved at the end of parsing."""
        loc = self.scanner.token
        self.scanner.scan()
        return loc

    def locexprs(self):
        accum = []
        accum.append(self.locexpr())
        while self.scanner.consume(','):
            accum.append(self.locexpr())
        return accum

    def locexpr(self, forward=False):
        if self.scanner.token in ('on', 'off'):
            loc = ConstantRef(TYPE_BIT, 1 if self.scanner.token == 'on' else 0)
            self.scanner.scan()
            return loc
        elif self.scanner.on_type('integer literal'):
            value = int(self.scanner.token)
            type_ = TYPE_WORD if value > 255 else TYPE_BYTE
            loc = ConstantRef(type_, value)
            self.scanner.scan()
            return loc
        elif self.scanner.consume('word'):
            loc = ConstantRef(TYPE_WORD, int(self.scanner.token))
            self.scanner.scan()
            return loc
        elif forward:
            name = self.scanner.token
            self.scanner.scan()
            loc = self.soft_lookup(name)
            if loc is not None:
                return loc
            else:
                return name
        else:
            loc = self.lookup(self.scanner.token)
            self.scanner.scan()
            return loc

    def indlocexpr(self, forward=False):
        if self.scanner.consume('['):
            loc = self.locexpr()
            self.scanner.expect(']')
            self.scanner.expect('+')
            self.scanner.expect('y')
            return IndirectRef(loc)
        elif self.scanner.consume('^'):
            loc = self.locexpr()
            return AddressRef(loc)
        else:
            return self.indexed_locexpr(forward=forward)

    def indexed_locexpr(self, forward=False):
        loc = self.locexpr(forward=forward)
        if not isinstance(loc, basestring):
            index = None
            if self.scanner.consume('+'):
                index = self.locexpr()
                loc = IndexedRef(loc, index)
        return loc

    def statics(self):
        defns = []
        while self.scanner.consume('static'):
            defn = self.defn()
            if defn.initial is None:
                raise SyntaxError("Static definition {} must have initial value".format(defn))
            defns.append(defn)
        return defns

    def block(self):
        instrs = []
        self.scanner.expect('{')
        while not self.scanner.on('}'):
            instrs.append(self.instr())
        self.scanner.expect('}')
        return Block(instrs=instrs)

    def instr(self):
        if self.scanner.consume('if'):
            inverted = False
            if self.scanner.consume('not'):
                inverted = True
            src = self.locexpr()
            block1 = self.block()
            block2 = None
            if self.scanner.consume('else'):
                block2 = self.block()
            return IfOp(src=src, block1=block1, block2=block2, inverted=inverted)
        elif self.scanner.consume('repeat'):
            inverted = False
            src = None
            block = self.block()
            if self.scanner.consume('until'):
                if self.scanner.consume('not'):
                    inverted = True
                src = self.locexpr()
            else:
                self.scanner.expect('forever')
            return BlockOp(opcode='repeat', src=src, block=block, inverted=inverted)
        elif self.scanner.token in ("ld",):
            # the same as add, sub, cmp etc below, except supports an indlocexpr for the src
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            self.scanner.expect(',')
            src = self.indlocexpr()
            return SingleOp(opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("add", "sub", "cmp", "and", "or", "xor"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            self.scanner.expect(',')
            src = self.indexed_locexpr()
            return SingleOp(opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("st",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.locexpr()
            self.scanner.expect(',')
            dest = self.indlocexpr()
            return SingleOp(opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("shl", "shr", "inc", "dec"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            return SingleOp(opcode=opcode, dest=dest, src=None)
        elif self.scanner.token in ("call", "goto"):
            opcode = self.scanner.token
            self.scanner.scan()
            name = self.scanner.token
            self.scanner.scan()
            instr = SingleOp(opcode=opcode, location=name, dest=None, src=None)
            self.backpatch_instrs.append(instr)
            return instr
        elif self.scanner.token in ("copy",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.indlocexpr(forward=True)
            self.scanner.expect(',')
            dest = self.indlocexpr()
            instr = SingleOp(opcode=opcode, dest=dest, src=src)
            self.backpatch_instrs.append(instr)
            return instr
        elif self.scanner.consume("with"):
            self.scanner.expect("interrupts")
            self.scanner.expect("off")
            block = self.block()
            return BlockOp(opcode='with-sei', src=None, block=block)
        elif self.scanner.consume("trash"):
            dest = self.locexpr()
            return SingleOp(opcode='trash', src=None, dest=dest)
        else:
            raise ValueError('bad opcode "%s"' % self.scanner.token)
