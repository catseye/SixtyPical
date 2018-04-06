# encoding: UTF-8

from copy import copy

from sixtypical.model import RoutineType


class FallthruAnalyzer(object):

    def __init__(self, debug=False):
        self.debug = debug

    def analyze_program(self, program):
        self.program = program

        self.fallthru_map = {}
        for routine in program.routines:
            encountered_gotos = list(routine.encountered_gotos)
            if len(encountered_gotos) == 1 and isinstance(encountered_gotos[0].type, RoutineType):
                self.fallthru_map[routine.name] = encountered_gotos[0].name
            else:
                self.fallthru_map[routine.name] = None

    def find_chain(self, routine_name, available):
        chain = [routine_name]
        seen = set(chain)
        while True:
            next = self.fallthru_map.get(routine_name)
            if next is None or next in seen or next not in available:
                return chain
            seen.add(next)
            chain.append(next)
            routine_name = next

    def serialize(self):
        pending_routines = copy(self.fallthru_map)
        roster = []

        main_chain = self.find_chain('main', pending_routines)
        roster.append(main_chain)
        for k in main_chain:
            del pending_routines[k]

        while pending_routines:
            chains = [self.find_chain(k, pending_routines) for k in pending_routines.keys()]
            chains.sort(key=len, reverse=True)
            c = chains[0]
            roster.append(c)
            for k in c:
                del pending_routines[k]

        return roster
