SixtyPical Fallthru
===================

This is a test suite, written in [Falderal][] format, for SixtyPical's
ability to detect which routines make tail calls to other routines,
and thus can be re-arranged to simply "fall through" to them.

The theory is as follows.

SixtyPical supports a `goto`, but it can only appear in tail position.
If a routine r1 ends with a unique `goto` to a fixed routine r2 it is said
to *potentially fall through* to r2.

A *unique* `goto` means that there are not multiple different `goto`s in
tail position (which can happen if, for example, an `if` is the last thing
in a routine, and each branch of that `if` ends with a different `goto`.)

A *fixed* routine means, a routine which is known at compile time, not a
`goto` through a vector.

Consider the set R of all routines in the program.

Every routine r1 ∈ R either potentially falls through to a single routine
r2 ∈ R (r2 ≠ r1) or it does not potentially fall through to any routine.
We can say out(r1) = {r2} or out(r1) = ∅.

Every routine r ∈ R in this set also has a set of zero or more
routines from which it is potentially falled through to by.  Call this
in(r).  It is the case that out(r1) = {r2} → r1 ∈ in(r2).

We can trace out the connections by following the in- or our- sets of
a given routine.  Because each routine potentially falls through to only
a single routine, the structures we find will be tree-like, not DAG-like.

But they do permit cycles.

So, we first break those cycles.  We will be left with out() sets which
are disjoint trees, i.e. if r1 ∈ in(r2), then r1 ∉ in(r3) for all r3 ≠ r2.

We then follow an algorithm something like this.  Treat R as a mutable
set and start with an empty list L.  Then,

- Pick a routine r from R where out(r) = ∅.
- Find the longest chain of routines r1,r2,...rn in R where out(r1) = {r2},
  out(r2} = {r3}, ... out(rn-1) = {rn}, and rn = r.
- Remove (r1,r2,...,rn) from R and append them to L in that order.
  Mark (r1,r2,...rn-1) as "will have their final `goto` removed."
- Repeat until R is empty.

When times comes to generate code, generate it in the order given by L.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Dump fallthru info for SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --optimize-fallthru --dump-fallthru-info --analyze-only --traceback %(test-body-file)"

    -> Tests for functionality "Dump fallthru info for SixtyPical program"

A single routine, obviously, falls through to nothing and has nothing fall
through to it.

    | define main routine
    | {
    | }
    = {}

If main does a `goto foo`, then it can fall through to `foo`.

    | define foo routine trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define main routine trashes a, z, n
    | {
    |     goto foo
    | }
    = {
    =     "foo": [
    =         "main"
    =     ]
    = }

More than one routine can fall through to a routine.

If main does a `goto foo`, then it can fall through to `foo`.

    | define foo routine trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define bar routine trashes a, z, n
    | {
    |     ld a, 0
    |     goto foo
    | }
    | 
    | define main routine trashes a, z, n
    | {
    |     goto foo
    | }
    = {
    =     "foo": [
    =         "bar", 
    =         "main"
    =     ]
    = }

There is nothing stopping two routines from tail-calling each
other, but we will only be able to make one of them, at most,
fall through to the other.

    | define foo routine trashes a, z, n
    | {
    |     ld a, 0
    |     goto bar
    | }
    | 
    | define bar routine trashes a, z, n
    | {
    |     ld a, 0
    |     goto foo
    | }
    | 
    | define main routine trashes a, z, n
    | {
    | }
    = {
    =     "bar": [
    =         "foo"
    =     ], 
    =     "foo": [
    =         "bar"
    =     ]
    = }

If a routine does two tail calls (which is possible because they
can be in different branches of an `if`) it cannot fall through to another
routine.

    | define foo routine trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define bar routine trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define main routine inputs z trashes a, z, n
    | {
    |     if z {
    |         goto foo
    |     } else {
    |         goto bar
    |     }
    | }
    = {}

Similarly, a tail call to a vector can't be turned into a fallthru,
because we don't necessarily know what actual routine the vector contains.

    | vector routine trashes a, z, n
    |   vec
    | 
    | define foo routine trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define bar routine trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define main routine outputs vec trashes a, z, n
    | {
    |     copy bar, vec
    |     goto vec
    | }
    = {}
