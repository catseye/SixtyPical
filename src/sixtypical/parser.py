# Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
# This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
# SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical

# encoding: UTF-8

from sixtypical.ast import (
    Program, Defn, Routine, Block, SingleOp, Reset, Call, GoTo, If, Repeat, For, WithInterruptsOff, Save, PointInto
)
from sixtypical.model import (
    TYPE_BIT, TYPE_BYTE, TYPE_WORD,
    RoutineType, VectorType, TableType, PointerType,
    ConstantRef, IndirectRef, IndexedRef,
)
from sixtypical.scanner import Scanner
from sixtypical.symtab import SymEntry


class ForwardReference(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, self.name)


class Parser(object):
    def __init__(self, symtab, text, filename, include_path):
        self.symtab = symtab
        self.include_path = include_path
        self.scanner = Scanner(text, filename)
        self.current_routine_name = None

    def syntax_error(self, msg):
        self.scanner.syntax_error(msg)

    def lookup(self, name, allow_forward=False, routine_name=None):
        model = self.symtab.fetch_global_ref(name)
        if model is None and routine_name:
            model = self.symtab.fetch_local_ref(routine_name, name)
        if model is None and allow_forward:
            return ForwardReference(name)
        if model is None:
            self.syntax_error('Undefined symbol "{}"'.format(name))
        return model

    def declare(self, name, ast_node, type_):
        if self.symtab.fetch_global_ref(name):
            self.syntax_error('Symbol "%s" already declared' % name)
        self.symtab.symbols[name] = SymEntry(ast_node, type_)

    def declare_local(self, routine_name, name, ast_node, type_):
        if self.symtab.fetch_local_ref(routine_name, name):
            self.syntax_error('Symbol "%s" already declared locally' % name)
        if self.symtab.fetch_global_ref(name):
            self.syntax_error('Symbol "%s" already declared globally' % name)
        self.symtab.locals.setdefault(routine_name, {})[name] = SymEntry(ast_node, type_)

    # ---- symbol resolution

    def resolve_symbols(self, program):
        # This could stand to be better unified.

        def resolve(w):
             return self.lookup(w.name) if isinstance(w, ForwardReference) else w

        def backpatched_type(type_):
            if isinstance(type_, TableType):
                return TableType(backpatched_type(type_.of_type), type_.size)
            elif isinstance(type_, VectorType):
                return VectorType(backpatched_type(type_.of_type))
            elif isinstance(type_, RoutineType):
                return RoutineType(
                    frozenset([resolve(w) for w in type_.inputs]),
                    frozenset([resolve(w) for w in type_.outputs]),
                    frozenset([resolve(w) for w in type_.trashes]),
                )
            else:
                return type_

        for name, symentry in self.symtab.symbols.items():
            symentry.type_ = backpatched_type(symentry.type_)

        def resolve_fwd_reference(obj, field):
            field_value = getattr(obj, field, None)
            if isinstance(field_value, ForwardReference):
                setattr(obj, field, self.lookup(field_value.name))
            elif isinstance(field_value, IndexedRef):
                if isinstance(field_value.ref, ForwardReference):
                    field_value.ref = self.lookup(field_value.ref.name)

        for node in program.all_children():
            if isinstance(node, SingleOp):
                resolve_fwd_reference(node, 'src')
                resolve_fwd_reference(node, 'dest')
            if isinstance(node, (Call, GoTo)):
                resolve_fwd_reference(node, 'location')

    # --- grammar productions

    def program(self):
        defns = []
        routines = []
        includes = []
        while self.scanner.consume('include'):
            filename = self.scanner.token
            self.scanner.scan()
            program = load_program(filename, self.symtab, self.include_path, include_file=True)
            includes.append(program)
        while self.scanner.on('typedef', 'const'):
            if self.scanner.on('typedef'):
                self.typedef()
            if self.scanner.on('const'):
                self.defn_const()
        typenames = ['byte', 'word', 'table', 'vector', 'pointer']  # 'routine',
        typenames.extend(self.symtab.typedefs.keys())
        while self.scanner.on(*typenames):
            type_, defn = self.defn()
            self.declare(defn.name, defn, type_)
            defns.append(defn)
        while self.scanner.consume('define'):
            name = self.scanner.token
            self.scanner.scan()
            self.current_routine_name = name
            preserved = False
            if self.scanner.consume('preserved'):
                preserved = True
            type_, routine = self.routine(name)
            self.declare(name, routine, type_)
            routine.preserved = preserved
            routines.append(routine)
            self.current_routine_name = None
        self.scanner.check_type('EOF')

        program = Program(self.scanner.line_number, defns=defns, routines=routines)
        programs = includes + [program]
        program = merge_programs(programs)

        self.resolve_symbols(program)
        return program

    def typedef(self):
        self.scanner.expect('typedef')
        type_ = self.defn_type()
        name = self.defn_name()
        if name in self.symtab.typedefs:
            self.syntax_error('Type "%s" already declared' % name)
        self.symtab.typedefs[name] = type_
        return type_

    def defn_const(self):
        self.scanner.expect('const')
        name = self.defn_name()
        if name in self.symtab.consts:
            self.syntax_error('Const "%s" already declared' % name)
        loc = self.const()
        self.symtab.consts[name] = loc
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

        return type_, Defn(self.scanner.line_number, name=name, addr=addr, initial=initial)

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
        elif self.scanner.token in self.symtab.consts:
            loc = self.symtab.consts[self.scanner.token]
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
            if size <= 0 or size > 65536:
                self.syntax_error("Table size must be > 0 and <= 65536")
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
            type_ = RoutineType(frozenset(inputs), frozenset(outputs), frozenset(trashes))
        elif self.scanner.consume('pointer'):
            type_ = PointerType()
        else:
            type_name = self.scanner.token
            self.scanner.scan()
            if type_name not in self.symtab.typedefs:
                self.syntax_error("Undefined type '%s'" % type_name)
            type_ = self.symtab.typedefs[type_name]

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
        return (
            set([ForwardReference(n) for n in inputs]),
            set([ForwardReference(n) for n in outputs]),
            set([ForwardReference(n) for n in trashes])
        )

    def routine(self, name):
        type_ = self.defn_type()
        if not isinstance(type_, RoutineType):
            self.syntax_error("Can only define a routine, not {}".format(repr(type_)))
        locals_ = []
        if self.scanner.consume('@'):
            self.scanner.check_type('integer literal')
            block = None
            addr = int(self.scanner.token)
            self.scanner.scan()
        else:
            locals_ = self.locals()
            block = self.block()
            addr = None
        return type_, Routine(self.scanner.line_number, name=name, block=block, addr=addr, locals=locals_)

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
        if self.scanner.token in ('on', 'off', 'word') or self.scanner.token in self.symtab.consts or self.scanner.on_type('integer literal'):
            return self.const()
        else:
            name = self.scanner.token
            self.scanner.scan()
            return self.lookup(name, allow_forward=True, routine_name=self.current_routine_name)

    def indlocexpr(self):
        if self.scanner.consume('['):
            loc = self.locexpr()
            self.scanner.expect(']')
            self.scanner.expect('+')
            self.scanner.expect('y')
            return IndirectRef(loc)
        else:
            return self.indexed_locexpr()

    def indexed_locexpr(self):
        loc = self.locexpr()
        if not isinstance(loc, str):
            index = None
            offset = ConstantRef(TYPE_BYTE, 0)
            if self.scanner.consume('+'):
                if self.scanner.token in self.symtab.consts or self.scanner.on_type('integer literal'):
                    offset = self.const()
                    self.scanner.expect('+')
                index = self.locexpr()
                loc = IndexedRef(loc, offset, index)
        return loc

    def locals(self):
        defns = []
        while self.scanner.consume('static'):
            type_, defn = self.defn()
            if defn.initial is None:
                self.syntax_error("Static definition {} must have initial value".format(defn))
            self.declare_local(self.current_routine_name, defn.name, defn, type_)
            defns.append(defn)
        while self.scanner.consume('local'):
            type_, defn = self.defn()
            if defn.initial is not None:
                self.syntax_error("Local definition {} may not have initial value".format(defn))
            self.declare_local(self.current_routine_name, defn.name, defn, type_)
            defns.append(defn)
        return defns

    def block(self):
        instrs = []
        self.scanner.expect('{')
        while not self.scanner.on('}'):
            instrs.append(self.instr())
            if isinstance(instrs[-1], GoTo):
                break
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
        elif self.scanner.consume('reset'):
            pointer = self.locexpr()
            offset = self.const()
            return Reset(self.scanner.line_number, pointer=pointer, offset=offset)
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
        elif self.scanner.consume("call"):
            name = self.scanner.token
            self.scanner.scan()
            instr = Call(self.scanner.line_number, location=ForwardReference(name))
            return instr
        elif self.scanner.consume("goto"):
            name = self.scanner.token
            self.scanner.scan()
            instr = GoTo(self.scanner.line_number, location=ForwardReference(name))
            return instr
        elif self.scanner.token in ("copy",):
            opcode = self.scanner.token
            self.scanner.scan()
            src = self.indlocexpr()
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
        elif self.scanner.consume("point"):
            pointer = self.locexpr()
            self.scanner.expect("into")
            table = self.locexpr()
            block = self.block()
            return PointInto(self.scanner.line_number, pointer=pointer, table=table, block=block)
        elif self.scanner.consume("trash"):
            dest = self.locexpr()
            return SingleOp(self.scanner.line_number, opcode='trash', src=None, dest=dest)
        else:
            self.syntax_error('bad opcode "%s"' % self.scanner.token)


# - - - -


def load_program(filename, symtab, include_path, include_file=False):
    import os
    if include_file:
        for include_dir in include_path:
            if os.path.exists(os.path.join(include_dir, filename)):
                filename = os.path.join(include_dir, filename)
                break
    text = open(filename).read()
    parser = Parser(symtab, text, filename, include_path)
    program = parser.program()
    return program


def merge_programs(programs):
    """Assumes that the programs do not have any conflicts."""

    full = Program(1, defns=[], routines=[])
    for p in programs:
        full.defns.extend(p.defns)
        full.routines.extend(p.routines)

    return full
