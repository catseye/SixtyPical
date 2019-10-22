# encoding: UTF-8

from sixtypical.model import (
    RoutineType, ConstantRef, LocationRef, IndirectRef, IndexedRef,
)


class AnalysisContext(object):
    """
    A location is touched if it was changed (or even potentially
    changed) during this routine, or some routine called by this routine.

    A location is meaningful if it was an input to this routine,
    or if it was set to a meaningful value by some operation in this
    routine (or some routine called by this routine).

    If a location is meaningful, it has a range.  This range represents
    the lowest and highest values that it might possibly be (i.e. we know
    it cannot possibly be below the lowest or above the highest.)  In the
    absence of any usage information, the range of a byte, is 0..255 and
    the range of a word is 0..65535.

    A location is writeable if it was listed in the outputs and trashes
    lists of this routine.  A location can also be temporarily marked
    unwriteable in certain contexts, such as `for` loops.
    """
    def __init__(self, symtab, routine, inputs, outputs, trashes):
        from sixtypical.analyzer import ConstantConstraintError, InconsistentConstraintsError

        self.symtab = symtab
        self.routine = routine           # Routine (AST node)
        self._touched = set()            # {LocationRef}
        self._range = dict()             # LocationRef -> (Int, Int)
        self._writeable = set()          # {LocationRef}
        self._terminated = False
        self._gotos_encountered = set()
        self._pointer_assoc = dict()

        for ref in inputs:
            if self.is_constant(ref):
                raise ConstantConstraintError(self.routine, ref.name)
            self._range[ref] = self.max_range(ref)
        output_names = set()
        for ref in outputs:
            if self.is_constant(ref):
                raise ConstantConstraintError(self.routine, ref.name)
            output_names.add(ref.name)
            self._writeable.add(ref)
        for ref in trashes:
            if self.is_constant(ref):
                raise ConstantConstraintError(self.routine, ref.name)
            if ref.name in output_names:
                raise InconsistentConstraintsError(self.routine, ref.name)
            self._writeable.add(ref)

    def __str__(self):
        return "{}(\n  _touched={},\n  _range={},\n  _writeable={}\n)".format(
            self.__class__.__name__,
            LocationRef.format_set(self._touched), LocationRef.format_set(self._range), LocationRef.format_set(self._writeable)
        )

    def to_json_data(self):
        type_ = self.symtab.fetch_global_type(self.routine.name)
        return {
            'routine_inputs': ','.join(sorted(loc.name for loc in type_.inputs)),
            'routine_outputs': ','.join(sorted(loc.name for loc in type_.outputs)),
            'routine_trashes': ','.join(sorted(loc.name for loc in type_.trashes)),
            'touched': ','.join(sorted(loc.name for loc in self._touched)),
            'range': dict((loc.name, '{}-{}'.format(rng[0], rng[1])) for (loc, rng) in self._range.items()),
            'writeable': ','.join(sorted(loc.name for loc in self._writeable)),
            'terminated': self._terminated,
            'gotos_encountered': ','.join(sorted(loc.name for loc in self._gotos_encountered)),
        }

    def clone(self):
        c = AnalysisContext(self.symtab, self.routine, [], [], [])
        c._touched = set(self._touched)
        c._range = dict(self._range)
        c._writeable = set(self._writeable)
        c._pointer_assoc = dict(self._pointer_assoc)
        c._gotos_encountered = set(self._gotos_encountered)
        return c

    def update_from(self, other):
        """Replaces the information in this context, with the information from the other context.
        This is an overwriting action - it does not attempt to merge the contexts.

        We do not replace the gotos_encountered for technical reasons.  (In `analyze_if`,
        we merge those sets afterwards; at the end of `analyze_routine`, they are not distinct in the
        set of contexts we are updating from, and we want to retain our own.)"""
        self.routine = other.routine
        self._touched = set(other._touched)
        self._range = dict(other._range)
        self._writeable = set(other._writeable)
        self._terminated = other._terminated
        self._pointer_assoc = dict(other._pointer_assoc)

    def each_meaningful(self):
        for ref in self._range.keys():
            yield ref

    def each_touched(self):
        for ref in self._touched:
            yield ref

    def each_writeable(self):
        for ref in self._writeable:
            yield ref

    def assert_meaningful(self, *refs, **kwargs):
        from sixtypical.analyzer import UnmeaningfulReadError

        exception_class = kwargs.get('exception_class', UnmeaningfulReadError)
        for ref in refs:
            if self.symtab.has_local(self.routine.name, ref.name):
                if ref not in self._range:
                    message = ref.name
                    if kwargs.get('message'):
                        message += ' (%s)' % kwargs['message']
                    raise exception_class(self.routine, message)
                else:
                    continue
            if self.is_constant(ref):
                pass
            elif isinstance(ref, LocationRef):
                if ref not in self._range:
                    message = ref.name
                    if kwargs.get('message'):
                        message += ' (%s)' % kwargs['message']
                    raise exception_class(self.routine, message)
            elif isinstance(ref, IndexedRef):
                self.assert_meaningful(ref.ref, **kwargs)
                self.assert_meaningful(ref.index, **kwargs)
            else:
                raise NotImplementedError(ref)

    def assert_writeable(self, *refs, **kwargs):
        from sixtypical.analyzer import ForbiddenWriteError

        exception_class = kwargs.get('exception_class', ForbiddenWriteError)
        for ref in refs:
            # locals are always writeable
            if self.symtab.has_local(self.routine.name, ref.name):
                continue
            if ref not in self._writeable:
                message = ref.name
                if kwargs.get('message'):
                    message += ' (%s)' % kwargs['message']
                raise exception_class(self.routine, message)

    def assert_in_range(self, inside, outside, offset):
        """Given two locations, assert that the first location, offset by the given offset,
        is contained 'inside' the second location."""
        from sixtypical.analyzer import RangeExceededError

        assert isinstance(inside, LocationRef)
        assert isinstance(outside, LocationRef)

        # inside should always be meaningful
        inside_range = self._range[inside]

        # outside might not be meaningful, so default to max range if necessary
        if outside in self._range:
            outside_range = self._range[outside]
        else:
            outside_range = self.max_range(outside)

        if (inside_range[0] + offset.value) < outside_range[0] or (inside_range[1] + offset.value) > outside_range[1]:
            raise RangeExceededError(self.routine,
                "Possible range of {} {} (+{}) exceeds acceptable range of {} {}".format(
                    inside, inside_range, offset, outside, outside_range
                )
            )

    def set_touched(self, *refs):
        for ref in refs:
            self._touched.add(ref)
            # TODO: it might be possible to invalidate the range here

    def set_meaningful(self, *refs):
        for ref in refs:
            if ref not in self._range:
                self._range[ref] = self.max_range(ref)

    def set_top_of_range(self, ref, top):
        self.assert_meaningful(ref)
        (bottom, _) = self._range[ref]
        self._range[ref] = (bottom, top)

    def set_bottom_of_range(self, ref, bottom):
        self.assert_meaningful(ref)
        (top, _) = self._range[ref]
        self._range[ref] = (bottom, top)

    def set_range(self, ref, bottom, top):
        self.assert_meaningful(ref)
        self._range[ref] = (bottom, top)

    def get_top_of_range(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        self.assert_meaningful(ref)
        (_, top) = self._range[ref]
        return top

    def get_bottom_of_range(self, ref):
        if isinstance(ref, ConstantRef):
            return ref.value
        self.assert_meaningful(ref)
        (bottom, _) = self._range[ref]
        return bottom

    def get_range(self, ref):
        if isinstance(ref, ConstantRef):
            return (ref.value, ref.value)
        self.assert_meaningful(ref)
        (bottom, top) = self._range[ref]
        return bottom, top

    def copy_range(self, src, dest):
        self.assert_meaningful(src)
        if src in self._range:
            src_range = self._range[src]
        else:
            src_range = self.max_range(src)
        self._range[dest] = src_range

    def invalidate_range(self, ref):
        self.assert_meaningful(ref)
        self._range[ref] = self.max_range(ref)

    def set_unmeaningful(self, *refs):
        for ref in refs:
            if ref in self._range:
                del self._range[ref]

    def set_written(self, *refs):
        """A "helper" method which does the following common sequence for
        the given refs: asserts they're all writable, and sets them all
        as touched and meaningful."""
        self.assert_writeable(*refs)
        self.set_touched(*refs)
        self.set_meaningful(*refs)

    def set_unwriteable(self, *refs):
        """Intended to be used for implementing analyzing `for`."""
        for ref in refs:
            self._writeable.remove(ref)

    def set_writeable(self, *refs):
        """Intended to be used for implementing analyzing `for`, but also used in `save`."""
        for ref in refs:
            self._writeable.add(ref)

    def encounter_gotos(self, gotos):
        self._gotos_encountered |= gotos

    def encountered_gotos(self):
        return self._gotos_encountered

    def set_terminated(self):
        # Having a terminated context and having encountered gotos is not the same thing.
        self._terminated = True

    def has_terminated(self):
        return self._terminated

    def extract(self, location):
        """Sets the given location as writeable in the context, and returns a 'baton' representing
        the previous state of context for that location.  This 'baton' can be used to later restore
        this state of context."""
        # Used in `save`.
        baton = (
            location,
            location in self._touched,
            self._range.get(location, None),
            location in self._writeable,
        )
        self.set_writeable(location)
        return baton

    def re_introduce(self, baton):
        """Given a 'baton' produced by `extract()`, restores the context for that saved location
        to what it was before `extract()` was called."""
        # Used in `save`.
        location, was_touched, was_range, was_writeable = baton

        if was_touched:
            self._touched.add(location)
        elif location in self._touched:
            self._touched.remove(location)

        if was_range is not None:
            self._range[location] = was_range
        elif location in self._range:
            del self._range[location]

        if was_writeable:
            self._writeable.add(location)
        elif location in self._writeable:
            self._writeable.remove(location)

    def get_assoc(self, pointer):
        return self._pointer_assoc.get(pointer)

    def set_assoc(self, pointer, table):
        self._pointer_assoc[pointer] = table

    def is_constant(self, ref):
        """read-only means that the program cannot change the value
        of a location.  constant means that the value of the location
        will not change during the lifetime of the program."""
        if isinstance(ref, ConstantRef):
            return True
        if isinstance(ref, (IndirectRef, IndexedRef)):
            return False
        if isinstance(ref, LocationRef):
            type_ = self.symtab.fetch_global_type(ref.name)
            return isinstance(type_, RoutineType)
        raise NotImplementedError

    def max_range(self, ref):
        if isinstance(ref, ConstantRef):
            return (ref.value, ref.value)
        elif self.symtab.has_local(self.routine.name, ref.name):
            return self.symtab.fetch_local_type(self.routine.name, ref.name).max_range
        else:
            return self.symtab.fetch_global_type(ref.name).max_range
