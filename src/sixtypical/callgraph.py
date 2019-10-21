from sixtypical.model import RoutineType, VectorType


def find_routines_matching_vector_type(program, type_):
    return []  # dummy


def construct_callgraph(program):
    graph = {}

    for routine in program.routines:
        potentially_calls = []
        for (called_routine, called_routine_type) in routine.called_routines:
            if isinstance(called_routine_type, RoutineType):
                potentially_calls.append(called_routine.name)
            elif isinstance(called_routine_type, VectorType):
                for potentially_called in find_routines_matching_vector_type(program, called_routine_type):
                    potentially_calls.append(potentially_called.name)
            else:
                raise NotImplementedError
        graph[routine.name] = {
            'potentially-calls': potentially_calls,
        }

    # Reflexive closure

    for routine in program.routines:
        potentially_called_by = []
        for (name, node) in graph.items():
            potential_calls = node['potentially-calls']
            if routine.name in potential_calls:
                potentially_called_by.append(name)
        graph[routine.name]['potentially-called-by'] = potentially_called_by

    return graph
