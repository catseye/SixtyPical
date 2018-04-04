# encoding: UTF-8

from sixtypical.model import RoutineType


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

    def break_cycles(self):
        raise NotImplementedError

    def serialize(self):
        raise NotImplementedError
