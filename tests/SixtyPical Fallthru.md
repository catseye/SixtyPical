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

Consider the set R of all available routines in the program.

Every routine either potentially falls through to a single other routine
or it does not potentially fall through to any routine.

More formally, we can say

fall : R → R ∪ {nil}, fall(r) ≠ r

where `nil` is an atom that represents no routine.

Now consider an operation chain() vaguely similar to a transitive closure
on fall().  Starting with r, we construct a list of r, fall(r),
fall(fall(r)), ... with the following restrictions:

-   we stop when we reach `nil` (because fall(`nil`) is not defined)
-   we stop when we see an element that is not in R.
-   we stop when we see an element that we have already added to the
    list (this is to prevent infinite lists due to cycles.)

With these definitions, our algorithm is something like this.

Treat R as a mutable set and start with an empty list of lists L.  Then,

-   For all r ∈ R, find all chain(r).
-   Pick a longest such chain.  Call it C.
-   Append C to L.
-   Remove all elements occurring in C, from R.
-   Repeat until R is empty.

When times comes to generate code, generate it in the order given by L.
In addition, each sublist in L represents a number of routines to
generate; all except the final routine in such a sublist need not have
any jump instruction generated for its final `goto`.

The tests in this document test against the list L.

Note that this optimization is a feature of the SixtyPical's reference
compiler, not the language.  So an implementation is not required
to pass these tests to be considered an implementation of SixtyPical.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Dump fallthru info for SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --optimize-fallthru --dump-fallthru-info --analyze-only --traceback %(test-body-file)"

    -> Tests for functionality "Dump fallthru info for SixtyPical program"

A single routine, obviously, falls through to nothing and has nothing fall
through to it.

    | define main routine
    | {
    | }
    = [
    =     [
    =         "main"
    =     ]
    = ]

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
    = [
    =     [
    =         "main", 
    =         "foo"
    =     ]
    = ]

More than one routine can fall through to a routine.  We pick one
of them to fall through, when selecting the order of routines.

Also note, `main` is always serialized first, so that the entry
point of the entire program appears at the beginning of the code.

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
    = [
    =     [
    =         "main", 
    =         "foo"
    =     ], 
    =     [
    =         "bar"
    =     ]
    = ]

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
    = [
    =     [
    =         "main"
    =     ], 
    =     [
    =         "bar", 
    =         "foo"
    =     ]
    = ]

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
    = [
    =     [
    =         "main"
    =     ], 
    =     [
    =         "bar"
    =     ], 
    =     [
    =         "foo"
    =     ]
    = ]

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
    = [
    =     [
    =         "main"
    =     ], 
    =     [
    =         "bar"
    =     ], 
    =     [
    =         "foo"
    =     ]
    = ]
