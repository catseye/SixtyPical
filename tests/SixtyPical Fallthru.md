SixtyPical Fallthru
===================

This is a test suite, written in [Falderal][] format, for SixtyPical's
ability to detect which routines make tail calls to other routines,
and thus can be re-arranged to simply "fall through" to them.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Dump fallthru map of SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --analyze-only --dump-fallthru-map --traceback %(test-body-file)"

    -> Tests for functionality "Dump fallthru map of SixtyPical program"

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
