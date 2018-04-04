# encoding: UTF-8

from sixtypical.model import RoutineType


def make_transitive_closure(d, key, s):
    for sub in d.get(key, []):
        if sub not in s:
            s.add(sub)
            make_transitive_closure(d, sub, s)


class FallthruAnalyzer(object):

    def __init__(self, debug=False):
        self.debug = debug

    def analyze_program(self, program):
        fallthru_map = {}
        for routine in program.routines:
            encountered_gotos = list(routine.encountered_gotos)
            if len(encountered_gotos) == 1 and isinstance(encountered_gotos[0].type, RoutineType):
                fallthru_map.setdefault(encountered_gotos[0].name, set()).add(routine.name)
        self.fallthru_map = dict([(k, sorted(v)) for k, v in fallthru_map.iteritems()])
        return self.fallthru_map

    def find_ancestors(self):
        self.ancestor_map = {}
        for key in self.fallthru_map:
            ancestors = set()
            make_transitive_closure(self.fallthru_map, key, ancestors)
            self.ancestor_map[key] = sorted(ancestors)
        return self.ancestor_map

    def find_cycles(self):
        self.cycles_found = set()
        for key in self.ancestor_map:
            if key in self.ancestor_map[key]:
                self.cycles_found.add(key)
        return self.cycles_found

    def break_cycles(self):
        new_fallthru_map = {}
        for key in self.fallthru_map:
            values = set(self.fallthru_map[key]) - self.cycles_found
            if values:
                new_fallthru_map[key] = sorted(values)
        self.fallthru_map = new_fallthru_map

    def serialize(self):
        raise NotImplementedError
