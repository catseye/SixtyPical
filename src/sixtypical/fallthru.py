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
        fall_in_map = {}
        for routine in program.routines:
            encountered_gotos = list(routine.encountered_gotos)
            if len(encountered_gotos) == 1 and isinstance(encountered_gotos[0].type, RoutineType):
                fall_in_map.setdefault(encountered_gotos[0].name, set()).add(routine.name)
        self.fall_in_map = dict([(k, sorted(v)) for k, v in fall_in_map.iteritems()])
        return self.fall_in_map

    def find_cycles(self):
        self.ancestor_map = {}
        for key in self.fall_in_map:
            ancestors = set()
            make_transitive_closure(self.fall_in_map, key, ancestors)
            self.ancestor_map[key] = sorted(ancestors)

        self.cycles_found = set()
        for key in self.ancestor_map:
            if key in self.ancestor_map[key]:
                self.cycles_found.add(key)

        return self.cycles_found

    def break_cycle(self):
        cycle_to_break = sorted(self.cycles_found)[0]
        cycles_to_break = set([cycle_to_break])

        new_fall_in_map = {}
        for key in self.fall_in_map:
            values = set(self.fall_in_map[key]) - cycles_to_break
            if values:
                new_fall_in_map[key] = sorted(values)
        self.fall_in_map = new_fall_in_map

    def serialize(self):
        raise NotImplementedError
