# encoding: UTF-8

import re

from sixtypical.ast import Program, Defn, Routine, Block, Instr
from sixtypical.model import (
    TYPE_BIT, TYPE_BYTE, TYPE_BYTE_TABLE,
    RoutineType, VectorType, ExecutableType,
    LocationRef, ConstantRef
)


class Scanner(object):
    def __init__(self, text):
        self.text = text
        self.token = None
        self.type = None
        self.scan()

    def scan_pattern(self, pattern, type, token_group=1, rest_group=2):
        pattern = r'^(' + pattern + r')(.*?)$'
        match = re.match(pattern, self.text, re.DOTALL)
        if not match:
            return False
        else:
            self.type = type
            self.token = match.group(token_group)
            self.text = match.group(rest_group)
            return True

    def scan(self):
        self.scan_pattern(r'[ \t\n\r]*', 'whitespace')
        while self.scan_pattern(r'\/\/.*?[\n\r]', 'comment'):
            self.scan_pattern(r'[ \t\n\r]*', 'whitespace')
        if not self.text:
            self.token = None
            self.type = 'EOF'
            return
        if self.scan_pattern(r'\,|\@|\+|\:|\{|\}', 'operator'):
            return
        if self.scan_pattern(r'\d+', 'integer literal'):
            return
        if self.scan_pattern(r'\$([0-9a-fA-F]+)', 'integer literal',
                             token_group=2, rest_group=3):
            # ecch
            self.token = str(eval('0x' + self.token))
            return
        if self.scan_pattern(r'\"(.*?)\"', 'string literal',
                             token_group=2, rest_group=3):
            return
        if self.scan_pattern(r'\w+', 'identifier'):
            return
        if self.scan_pattern(r'.', 'unknown character'):
            return
        else:
            raise AssertionError("this should never happen, self.text=(%s)" % self.text)

    def expect(self, token):
        if self.token == token:
            self.scan()
        else:
            raise SyntaxError("Expected '%s', but found '%s'" %
                              (token, self.token))

    def on(self, token):
        return self.token == token

    def on_type(self, type):
        return self.type == type

    def check_type(self, type):
        if not self.type == type:
            raise SyntaxError("Expected %s, but found %s ('%s')" %
                              (type, self.type, self.token))

    def consume(self, token):
        if self.token == token:
            self.scan()
            return True
        else:
            return False


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

    def lookup(self, name):
        if name not in self.symbols:
            raise SyntaxError('Undefined symbol "%s"' % name)
        return self.symbols[name].model

    def program(self):
        defns = []
        routines = []
        while self.scanner.on('byte') or self.scanner.on('vector'):
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
        return Program(defns=defns, routines=routines)

    def defn(self):
        type = TYPE_BYTE
        if self.scanner.consume('byte'):
            type = TYPE_BYTE
            if self.scanner.consume('table'):
                type = TYPE_BYTE_TABLE
        else:
            self.scanner.expect('vector')
            type = 'vector'
        self.scanner.check_type('identifier')
        name = self.scanner.token
        self.scanner.scan()

        (inputs, outputs, trashes) = self.constraints()
        if type == 'vector':
            type = VectorType(inputs=inputs, outputs=outputs, trashes=trashes)
        elif inputs or outputs or trashes:
            raise SyntaxError("Cannot apply constraints to non-vector type")

        initial = None
        if self.scanner.consume(':'):
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

        location = LocationRef(type, name)

        return Defn(name=name, addr=addr, initial=initial, location=location)

    def constraints(self):
        inputs = set()
        outputs = set()
        trashes = set()
        if self.scanner.consume('inputs'):
            inputs = set(self.locexprs())
        if self.scanner.consume('outputs'):
            outputs = set(self.locexprs())
        if self.scanner.consume('trashes'):
            trashes = set(self.locexprs())
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
            loc = ConstantRef(TYPE_BYTE, int(self.scanner.token))
            self.scanner.scan()
            return loc
        else:
            loc = self.lookup(self.scanner.token)
            self.scanner.scan()
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
            if name not in self.symbols:
                raise SyntaxError('Undefined routine "%s"' % name)
            if not isinstance(self.symbols[name].model.type, ExecutableType):
                raise SyntaxError('Illegal call of non-executable "%s"' % name)
            return Instr(opcode=opcode, location=self.symbols[name].model, dest=None, src=None)
        elif self.scanner.token in ("copy",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.locexpr()
            self.scanner.expect(',')
            dest = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=src)
        elif self.scanner.consume("with"):
            self.scanner.expect("interrupts")
            self.scanner.expect("off")
            block = self.block()
            return Instr(opcode='with-sei', dest=None, src=None, block=block)
        else:
            raise ValueError('bad opcode "%s"' % self.scanner.token)
