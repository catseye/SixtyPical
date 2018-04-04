# encoding: UTF-8

from copy import copy

from sixtypical.model import RoutineType


def make_transitive_closure(d, key, s):
    for sub in d.get(key, []):
        if sub not in s:
            s.add(sub)
            make_transitive_closure(d, sub, s)


def find_chains(d, key, pred):
    chains = []
    for sub in d.get(key, []):
        if pred(sub):
            subchains = find_chains(d, sub, pred)
            for subchain in subchains:
                chains.append([key] + subchain)
    chains.append([key])
    return chains


class FallthruAnalyzer(object):

    def __init__(self, debug=False):
        self.debug = debug

    def analyze_program(self, program):
        self.program = program
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
        pending_routines = sorted(self.fall_in_map.keys())
        routine_names = sorted([routine.name for routine in self.program.routines])
        for routine_name in routine_names:
            if routine_name not in pending_routines:
                pending_routines.append(routine_name)

        roster = []
        while pending_routines:
            # Pick a routine that is still pending to be serialized.
            key = pending_routines[0]

            in_set = self.fall_in_map.get(key, [])

            # Find the longest chain of routines r1,r2,...rn in R
            # where out(r1) = {r2}, out(r2} = {r3}, ... out(rn-1) = {rn}, and rn = r.
            chains = find_chains(self.fall_in_map, key, lambda k: k in pending_routines)
            chains.sort(key=len, reverse=True)
            routines = chains[0]
            routines.reverse()

            # Append (r1,r2,...,rn) to the roster and remove r1,r2,...rn from R.
            # A sublist like this appearing in the roster has meaning
            # "optimize the final goto out of all but the last routine in the sublist".
            for r in routines:
                pending_routines.remove(r)
            roster.append(routines)

        return roster
