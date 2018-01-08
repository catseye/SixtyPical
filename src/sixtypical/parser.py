# encoding: UTF-8

from sixtypical.ast import Program, Defn, Routine, Block, Instr
from sixtypical.model import (
    TYPE_BIT, TYPE_BYTE, TYPE_BYTE_TABLE, TYPE_WORD, TYPE_WORD_TABLE,
    RoutineType, VectorType, ExecutableType, BufferType, PointerType,
    LocationRef, ConstantRef, IndirectRef, IndexedRef, AddressRef,
)
from sixtypical.scanner import Scanner


class SymEntry(object):
    def __init__(self, ast_node, model):
        self.ast_node = ast_node
        self.model = model


class Parser(object):
    def __init__(self, text):
        self.scanner = Scanner(text)
        self.symbols = {}  # token -> SymEntry
        for token in ('a', 'x', 'y'):
            self.symbols[token] = SymEntry(None, LocationRef(TYPE_BYTE, token))
        for token in ('c', 'z', 'n', 'v'):
            self.symbols[token] = SymEntry(None, LocationRef(TYPE_BIT, token))
        self.backpatch_instrs = []

    def lookup(self, name):
        if name not in self.symbols:
            raise SyntaxError('Undefined symbol "%s"' % name)
        return self.symbols[name].model

    # --- grammar productions

    def program(self):
        defns = []
        routines = []
        while self.scanner.on('byte', 'word', 'vector', 'buffer', 'pointer'):
            defn = self.defn()
            name = defn.name
            if name in self.symbols:
                raise SyntaxError('Symbol "%s" already declared' % name)
            self.symbols[name] = SymEntry(defn, defn.location)
            defns.append(defn)
        while self.scanner.on('routine'):
            routine = self.routine()
            name = routine.name
            if name in self.symbols:
                raise SyntaxError('Symbol "%s" already declared' % name)
            self.symbols[name] = SymEntry(routine, routine.location)
            routines.append(routine)
        self.scanner.check_type('EOF')

        # now backpatch the executable types.
        for defn in defns:
            defn.location.backpatch_vector_labels(lambda w: self.lookup(w))
        for routine in routines:
            routine.location.backpatch_vector_labels(lambda w: self.lookup(w))
        for instr in self.backpatch_instrs:
            if instr.opcode in ('call', 'goto'):
                name = instr.location
                if name not in self.symbols:
                    raise SyntaxError('Undefined routine "%s"' % name)
                if not isinstance(self.symbols[name].model.type, ExecutableType):
                    raise SyntaxError('Illegal call of non-executable "%s"' % name)
                instr.location = self.symbols[name].model
            if instr.opcode in ('copy',) and isinstance(instr.src, basestring):
                name = instr.src
                if name not in self.symbols:
                    raise SyntaxError('Undefined routine "%s"' % name)
                if not isinstance(self.symbols[name].model.type, ExecutableType):
                    raise SyntaxError('Illegal copy of non-executable "%s"' % name)
                instr.src = self.symbols[name].model

        return Program(defns=defns, routines=routines)

    def defn(self):
        type_ = self.defn_type()

        self.scanner.check_type('identifier')
        name = self.scanner.token
        self.scanner.scan()

        (inputs, outputs, trashes) = self.constraints()
        if type_ == 'vector':
            type_ = VectorType(inputs=inputs, outputs=outputs, trashes=trashes)
        elif inputs or outputs or trashes:
            raise SyntaxError("Cannot apply constraints to non-vector type")

        initial = None
        if self.scanner.consume(':'):
            if type_ == TYPE_BYTE_TABLE and self.scanner.on_type('string literal'):
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

    def defn_type(self):
        if self.scanner.consume('byte'):
            if self.scanner.consume('table'):
                return TYPE_BYTE_TABLE
            return TYPE_BYTE
        elif self.scanner.consume('word'):
            if self.scanner.consume('table'):
                return TYPE_WORD_TABLE
            return TYPE_WORD
        elif self.scanner.consume('vector'):
            return 'vector'  # will be resolved to a Type by caller
        elif self.scanner.consume('buffer'):
            self.scanner.expect('[')
            self.scanner.check_type('integer literal')
            size = int(self.scanner.token)
            self.scanner.scan()
            self.scanner.expect(']')
            return BufferType(size)
        else:
            self.scanner.expect('pointer')
            return PointerType()

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

    def routine(self):
        self.scanner.expect('routine')
        name = self.scanner.token
        self.scanner.scan()
        (inputs, outputs, trashes) = self.constraints()
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            block = None
            addr = int(self.scanner.token)
            self.scanner.scan()
        else:
            block = self.block()
            addr = None
        location = LocationRef(
            RoutineType(inputs=inputs, outputs=outputs, trashes=trashes),
            name
        )
        return Routine(
            name=name, block=block, addr=addr,
            location=location
        )

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

    def locexpr(self):
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
        else:
            loc = self.lookup(self.scanner.token)
            self.scanner.scan()
            return loc

    def indlocexpr(self):
        if self.scanner.consume('forward'):
            return self.label()
        elif self.scanner.consume('['):
            loc = self.locexpr()
            self.scanner.expect(']')
            self.scanner.expect('+')
            self.scanner.expect('y')
            return IndirectRef(loc)
        elif self.scanner.consume('^'):
            loc = self.locexpr()
            return AddressRef(loc)
        else:
            loc = self.locexpr()
            index = None
            if self.scanner.consume('+'):
                index = self.locexpr()
                loc = IndexedRef(loc, index)
            return loc

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
            return Instr(opcode='if', dest=None, src=src,
                         block1=block1, block2=block2, inverted=inverted)
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
            return Instr(opcode='repeat', dest=None, src=src,
                         block=block, inverted=inverted)
        elif self.scanner.token in ("ld", "add", "sub", "cmp", "and", "or", "xor"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            self.scanner.expect(',')
            src = self.locexpr()
            index = None
            if self.scanner.consume('+'):
                index = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=src, index=index)
        elif self.scanner.token in ("st",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.locexpr()
            self.scanner.expect(',')
            dest = self.locexpr()
            index = None
            if self.scanner.consume('+'):
                index = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=src, index=index)
        elif self.scanner.token in ("shl", "shr", "inc", "dec"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=None)
        elif self.scanner.token in ("call", "goto"):
            opcode = self.scanner.token
            self.scanner.scan()
            name = self.scanner.token
            self.scanner.scan()
            instr = Instr(opcode=opcode, location=name, dest=None, src=None)
            self.backpatch_instrs.append(instr)
            return instr
        elif self.scanner.token in ("copy",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.indlocexpr()
            self.scanner.expect(',')
            dest = self.indlocexpr()
            instr = Instr(opcode=opcode, dest=dest, src=src)
            self.backpatch_instrs.append(instr)
            return instr
        elif self.scanner.consume("with"):
            self.scanner.expect("interrupts")
            self.scanner.expect("off")
            block = self.block()
            return Instr(opcode='with-sei', dest=None, src=None, block=block)
        elif self.scanner.consume("trash"):
            dest = self.locexpr()
            return Instr(opcode='trash', src=None, dest=dest)
        else:
            raise ValueError('bad opcode "%s"' % self.scanner.token)
