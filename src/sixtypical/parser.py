# encoding: UTF-8

import re

from sixtypical.ast import Program, Defn, Routine, Block, Instr
from sixtypical.model import LocationRef, ConstantRef


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
        if not self.text:
            self.token = None
            self.type = 'EOF'
            return
        if self.scan_pattern(r'\,|\@|\{|\}', 'operator'):
            return
        if self.scan_pattern(r'\d+', 'integer literal'):
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


class Parser(object):
    def __init__(self, text):
        self.scanner = Scanner(text)
        self.symbols = {}

    def lookup(self, name):
        if name in self.symbols:
            return LocationRef(name)
        else:
            raise KeyError(name)

    def program(self):
        defns = []
        routines = []
        while self.scanner.on('byte'):
            defn = self.defn()
            name = defn.name
            if name in self.symbols:
                raise KeyError(name)
            self.symbols[name] = defn
            defns.append(defn)
        while self.scanner.on('routine'):
            routine = self.routine()
            name = routine.name
            if name in self.symbols:
                raise KeyError(name)
            self.symbols[name] = routine
            routines.append(routine)
        return Program(defns=defns, routines=routines)

    def defn(self):
        self.scanner.expect('byte')
        name = self.scanner.token
        self.scanner.scan()
        return Defn(name=name)

    def routine(self):
        self.scanner.expect('routine')
        name = self.scanner.token
        self.scanner.scan()
        inputs = []
        outputs = []
        trashes = []
        if self.scanner.consume('inputs'):
            inputs = self.locexprs()
        if self.scanner.consume('outputs'):
            outputs = self.locexprs()
        if self.scanner.consume('trashes'):
            trashes = self.locexprs()
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            block = None
            addr = int(self.scanner.token)
            self.scanner.scan()
        else:
            block = self.block()
            addr = None
        return Routine(
            name=name, inputs=inputs, outputs=outputs, trashes=trashes,
            block=block, addr=addr
        )

    def locexprs(self):
        accum = []
        accum.append(self.locexpr())
        while self.scanner.consume(','):
            accum.append(self.locexpr())
        return accum

    def locexpr(self):
        if self.scanner.token in ('a', 'x', 'y', 'c', 'z', 'n', 'v'):
            loc = LocationRef(self.scanner.token)
            self.scanner.scan()
            return loc
        elif self.scanner.token in ('on', 'off'):
            loc = ConstantRef(1 if self.scanner.token == 'on' else 0)
            self.scanner.scan()
            return loc
        elif self.scanner.on_type('integer literal'):
            loc = ConstantRef(int(self.scanner.token))
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
            return Instr(opcode='repeat', dest=None, src=src,
                         block=block, inverted=inverted)
        elif self.scanner.token in ("ld", "add", "sub", "cmp", "and", "or", "xor"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            self.scanner.expect(',')
            src = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("st",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.locexpr()
            self.scanner.expect(',')
            dest = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=src)
        elif self.scanner.token in ("shl", "shr", "inc", "dec"):
            opcode = self.scanner.token
            self.scanner.scan()
            dest = self.locexpr()
            return Instr(opcode=opcode, dest=dest, src=None)
        elif self.scanner.token in ("call",):
            opcode = self.scanner.token
            self.scanner.scan()
            name = self.scanner.token
            self.scanner.scan()
            # TODO: check that is has been defined
            return Instr(opcode=opcode, name=name, dest=None, src=None)
        else:
            raise ValueError('bad opcode')
