# encoding: UTF-8

from sixtypical.ast import Program, Defn, Routine, Block, SingleOp, If, Repeat, For, WithInterruptsOff, Save
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


class ForwardReference(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.name)


class ParsingContext(object):
    def __init__(self):
        self.symbols = {}          # token -> SymEntry
        self.statics = {}          # token -> SymEntry
        self.typedefs = {}         # token -> Type AST
        self.consts = {}           # token -> Loc

        for token in ('a', 'x', 'y'):
            self.symbols[token] = SymEntry(None, LocationRef(TYPE_BYTE, token))
        for token in ('c', 'z', 'n', 'v'):
            self.symbols[token] = SymEntry(None, LocationRef(TYPE_BIT, token))

    def __str__(self):
        return "Symbols: {}\nStatics: {}\nTypedefs: {}\nConsts: {}".format(self.symbols, self.statics, self.typedefs, self.consts)

    def fetch(self, name):
        if name in self.statics:
            return self.statics[name].model
        if name in self.symbols:
            return self.symbols[name].model
        return None


class Parser(object):
    def __init__(self, context, text, filename):
        self.context = context
        self.scanner = Scanner(text, filename)

    def syntax_error(self, msg):
        self.scanner.syntax_error(msg)

    def lookup(self, name):
        model = self.context.fetch(name)
        if model is None:
            self.syntax_error('Undefined symbol "{}"'.format(name))
        return model

    def declare(self, name, symentry, static=False):
        if self.context.fetch(name):
            self.syntax_error('Symbol "%s" already declared' % name)
        if static:
            self.context.statics[name] = symentry
        else:
            self.context.symbols[name] = symentry

    def clear_statics(self):
        self.context.statics = {}

    # --- grammar productions

    def program(self):
        defns = []
        routines = []
        while self.scanner.on('typedef', 'const'):
            if self.scanner.on('typedef'):
                self.typedef()
            if self.scanner.on('const'):
                self.defn_const()
        typenames = ['byte', 'word', 'table', 'vector', 'buffer', 'pointer']  # 'routine',
        typenames.extend(self.context.typedefs.keys())
        while self.scanner.on(*typenames):
            defn = self.defn()
            self.declare(defn.name, SymEntry(defn, defn.location))
            defns.append(defn)
        while self.scanner.on('define', 'routine'):
            if self.scanner.consume('define'):
                name = self.scanner.token
                self.scanner.scan()
                routine = self.routine(name)
            else:
                routine = self.legacy_routine()
                name = routine.name
            self.declare(name, SymEntry(routine, routine.location))
            routines.append(routine)
        self.scanner.check_type('EOF')

        # now backpatch the executable types.
        #for type_name, type_ in self.context.typedefs.items():
        #    type_.backpatch_constraint_labels(lambda w: self.lookup(w))
        for defn in defns:
            defn.location.type.backpatch_constraint_labels(lambda w: self.lookup(w))
        for routine in routines:
            routine.location.type.backpatch_constraint_labels(lambda w: self.lookup(w))

        program = Program(self.scanner.line_number, defns=defns, routines=routines)

        for node in program.all_children():
            if isinstance(node, SingleOp):
                instr = node
                if instr.opcode in ('call', 'goto'):
                    forward_reference = instr.location
                    name = forward_reference.name
                    model = self.lookup(name)
                    if not isinstance(model.type, (RoutineType, VectorType)):
                        self.syntax_error('Illegal call of non-executable "%s"' % name)
                    instr.location = model
                if instr.opcode in ('copy',) and isinstance(instr.src, ForwardReference):
                    forward_reference = instr.src
                    name = forward_reference.name
                    model = self.lookup(name)
                    if not isinstance(model.type, (RoutineType, VectorType)):
                        self.syntax_error('Illegal copy of non-executable "%s"' % name)
                    instr.src = model

        return program

    def typedef(self):
        self.scanner.expect('typedef')
        type_ = self.defn_type()
        name = self.defn_name()
        if name in self.context.typedefs:
            self.syntax_error('Type "%s" already declared' % name)
        self.context.typedefs[name] = type_
        return type_

    def defn_const(self):
        self.scanner.expect('const')
        name = self.defn_name()
        if name in self.context.consts:
            self.syntax_error('Const "%s" already declared' % name)
        loc = self.const()
        self.context.consts[name] = loc
        return loc

    def defn(self):
        type_ = self.defn_type()
        name = self.defn_name()

        initial = None
        if self.scanner.consume(':'):
            if isinstance(type_, TableType):
                if self.scanner.on_type('string literal'):
                    initial = self.scanner.token
                    self.scanner.scan()
                else:
                    initial = []
                    initial.append(self.const().value)
                    while self.scanner.consume(','):
                        initial.append(self.const().value)
            else:
                initial = self.const().value

        addr = None
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            addr = int(self.scanner.token)
            self.scanner.scan()

        if initial is not None and addr is not None:
            self.syntax_error("Definition cannot have both initial value and explicit address")

        location = LocationRef(type_, name)

        return Defn(self.scanner.line_number, name=name, addr=addr, initial=initial, location=location)

    def const(self):
        if self.scanner.token in ('on', 'off'):
            loc = ConstantRef(TYPE_BIT, 1 if self.scanner.token == 'on' else 0)
            self.scanner.scan()
            return loc
        elif self.scanner.on_type('integer literal'):
            value = int(self.scanner.token)
            self.scanner.scan()
            type_ = TYPE_WORD if value > 255 else TYPE_BYTE
            loc = ConstantRef(type_, value)
            return loc
        elif self.scanner.consume('word'):
            loc = ConstantRef(TYPE_WORD, int(self.scanner.token))
            self.scanner.scan()
            return loc
        elif self.scanner.token in self.context.consts:
            loc = self.context.consts[self.scanner.token]
            self.scanner.scan()
            return loc
        else:
            self.syntax_error('bad constant "%s"' % self.scanner.token)

    def defn_size(self):
        self.scanner.expect('[')
        size = self.const().value
        self.scanner.expect(']')
        return size

    def defn_type(self):
        type_ = self.defn_type_term()

        if self.scanner.consume('table'):
            size = self.defn_size()
            if size <= 0 or size > 256:
                self.syntax_error("Table size must be > 0 and <= 256")
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
                self.syntax_error("Vectors can only be of a routine, not %r" % type_)
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
            if type_name not in self.context.typedefs:
                self.syntax_error("Undefined type '%s'" % type_name)
            type_ = self.context.typedefs[type_name]

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
            self.scanner.line_number,
            name=name, block=block, addr=addr,
            location=location
        )

    def routine(self, name):
        type_ = self.defn_type()
        if not isinstance(type_, RoutineType):
            self.syntax_error("Can only define a routine, not %r" % type_)
        statics = []
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            block = None
            addr = int(self.scanner.token)
            self.scanner.scan()
        else:
            statics = self.statics()

            self.clear_statics()
            for defn in statics:
                self.declare(defn.name, SymEntry(defn, defn.location), static=True)
            block = self.block()
            self.clear_statics()

            addr = None
        location = LocationRef(type_, name)
        return Routine(
            self.scanner.line_number,
            name=name, block=block, addr=addr,
            location=location, statics=statics
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

    def locexpr(self, forward=False):
        if self.scanner.token in ('on', 'off', 'word') or self.scanner.token in self.context.consts or self.scanner.on_type('integer literal'):
            return self.const()
        elif forward:
            name = self.scanner.token
            self.scanner.scan()
            loc = self.context.fetch(name)
            if loc is not None:
                return loc
            else:
                return ForwardReference(name)
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
        if not isinstance(loc, str):
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
                self.syntax_error("Static definition {} must have initial value".format(defn))
            defns.append(defn)
        return defns

    def block(self):
        instrs = []
        self.scanner.expect('{')
        while not self.scanner.on('}'):
            instrs.append(self.instr())
        self.scanner.expect('}')
        return Block(self.scanner.line_number, instrs=instrs)

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
            return If(self.scanner.line_number, src=src, block1=block1, block2=block2, inverted=inverted)
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
            return Repeat(self.scanner.line_number, src=src, block=block, inverted=inverted)
        elif self.scanner.consume('for'):
            dest = self.locexpr()
            if self.scanner.consume('down'):
                direction = -1
            elif self.scanner.consume('up'):
                direction = 1
            else:
                self.syntax_error('expected "up" or "down", found "%s"' % self.scanner.token)
            self.scanner.expect('to')
            final = self.const()
            block = self.block()
            return For(self.scanner.line_number, dest=dest, direction=direction, final=final, block=block)
        elif self.scanner.token in ("ld",):
            # the same as add, sub, cmp etc below, except supports an indlocexpr for the src
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            self.scanner.expect(',')
            src = self.indlocexpr()
            return SingleOp(self.scanner.line_number, opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("add", "sub", "cmp", "and", "or", "xor"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            self.scanner.expect(',')
            src = self.indexed_locexpr()
            return SingleOp(self.scanner.line_number, opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("st",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.locexpr()
            self.scanner.expect(',')
            dest = self.indlocexpr()
            return SingleOp(self.scanner.line_number, opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("shl", "shr", "inc", "dec"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.indexed_locexpr()
            return SingleOp(self.scanner.line_number, opcode=opcode, dest=dest, src=None)
        elif self.scanner.token in ("nop",):
            opcode = self.scanner.token
            self.scanner.scan()
            return SingleOp(self.scanner.line_number, opcode=opcode, dest=None, src=None)
        elif self.scanner.token in ("call", "goto"):
            opcode = self.scanner.token
            self.scanner.scan()
            name = self.scanner.token
            self.scanner.scan()
            instr = SingleOp(self.scanner.line_number, opcode=opcode, location=ForwardReference(name), dest=None, src=None)
            return instr
        elif self.scanner.token in ("copy",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.indlocexpr(forward=True)
            self.scanner.expect(',')
            dest = self.indlocexpr()
            instr = SingleOp(self.scanner.line_number, opcode=opcode, dest=dest, src=src)
            return instr
        elif self.scanner.consume("with"):
            self.scanner.expect("interrupts")
            self.scanner.expect("off")
            block = self.block()
            return WithInterruptsOff(self.scanner.line_number, block=block)
        elif self.scanner.consume("save"):
            locations = self.locexprs()
            block = self.block()
            return Save(self.scanner.line_number, locations=locations, block=block)
        elif self.scanner.consume("trash"):
            dest = self.locexpr()
            return SingleOp(self.scanner.line_number, opcode='trash', src=None, dest=dest)
        else:
            self.syntax_error('bad opcode "%s"' % self.scanner.token)


# - - - -


def merge_programs(programs):
    """Assumes that the programs do not have any conflicts."""

    full = Program(1, defns=[], routines=[])
    for p in programs:
        full.defns.extend(p.defns)
        full.routines.extend(p.routines)

    return full
