from sixtypical.model import RoutineType, VectorType


def find_routines_matching_type(program, type_):
    """Return the subset of routines of the program whose value
    may be assigned to a location of the given type_ (typically
    a vector)."""
    return [r for r in program.routines if RoutineType.executable_types_compatible(r.routine_type, type_)]


def construct_callgraph(program):
    graph = {}

    for routine in program.routines:
        potentially_calls = []
        for (called_routine, called_routine_type) in routine.called_routines:
            if isinstance(called_routine_type, RoutineType):
                potentially_calls.append(called_routine.name)
            elif isinstance(called_routine_type, VectorType):
                for potentially_called in find_routines_matching_type(program, called_routine_type):
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
        if getattr(routine, 'preserved', None) or routine.name == 'main':
            potentially_called_by.append('*preserved*')
        graph[routine.name]['potentially-called-by'] = potentially_called_by

    return graph
