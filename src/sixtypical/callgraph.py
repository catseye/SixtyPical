from sixtypical.ast import Program
from sixtypical.model import RoutineType, VectorType


def find_routines_matching_type(program, type_):
    """Return the subset of routines of the program whose value
    may be assigned to a location of the given type_ (typically
    a vector)."""
    return [r for r in program.routines if RoutineType.executable_types_compatible(r.routine_type, type_)]


def mark_as_reachable(graph, routine_name):
    node = graph[routine_name]
    if node.get('reachable', False):
        return
    node['reachable'] = True
    for next_routine_name in node['potentially-calls']:
        mark_as_reachable(graph, next_routine_name)


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
        graph[routine.name]['potentially-called-by'] = potentially_called_by

    # Root set

    root_set = set()

    for routine in program.routines:
        if getattr(routine, 'preserved', False) or routine.name == 'main':
            root_set.add(routine)

    # Reachability

    for routine in root_set:
        mark_as_reachable(graph, routine.name)

    return graph


def prune_unreachable_routines(program, callgraph):
    return Program(1, defns=program.defns, routines=[
        r for r in program.routines if callgraph[r.name].get('reachable', False)
    ])
