SixtyPical Callgraph
====================

This is a test suite, written in [Falderal][] format, for the ability of
a SixtyPical analyzer to construct a callgraph of which routines call which
other routines, and its ability to discover which routines will never be
called.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Dump callgraph info for SixtyPical program"

The `main` routine is always called.  The thing that it will
be called by is the system, but the callgraph analyzer simply
considers it to be "reachable".

    | define main routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [],
    =         "reachable": true
    =     }
    = }

If a routine is called by another routine, this fact will be noted.
If it is reachable (directly or indirectly) from `main`, this will
be noted as well.

    | define main routine
    | {
    |   call other
    | }
    | 
    | define other routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [
    =             "other"
    =         ],
    =         "reachable": true
    =     },
    =     "other": {
    =         "potentially-called-by": [
    =             "main"
    =         ],
    =         "potentially-calls": [],
    =         "reachable": true
    =     }
    = }

If a routine is not potentially called by any other routine that is
ultimately potentially called by `main`, this absence will be noted
— the routine will not be considered reachable — and a compiler or
linker will be permitted to omit it from the final executable.

    | define main routine
    | {
    | }
    | 
    | define other routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [],
    =         "reachable": true
    =     },
    =     "other": {
    =         "potentially-called-by": [],
    =         "potentially-calls": []
    =     }
    = }

If a routine is not called by another routine, but it is declared
explicitly as `preserved`, then it will still be considered
reachable, and a compiler or linker will not be permitted to omit it
from the final executable.  This is useful for interrupt routines
and such that really are used by some part of the system, even if
not directly by another SixtyPical routine.

    | define main routine
    | {
    | }
    | 
    | define other preserved routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [],
    =         "reachable": true
    =     },
    =     "other": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [],
    =         "reachable": true
    =     }
    = }

If a routine is called from a preserved routine, that routine is
reachable.

    | define main routine
    | {
    | }
    | 
    | define other1 preserved routine
    | {
    |   call other2
    | }
    | 
    | define other2 preserved routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [],
    =         "reachable": true
    =     },
    =     "other1": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [
    =             "other2"
    =         ],
    =         "reachable": true
    =     },
    =     "other2": {
    =         "potentially-called-by": [
    =             "other1"
    =         ],
    =         "potentially-calls": [],
    =         "reachable": true
    =     }
    = }

If a group of routines potentially call each other, but neither is
found to be reachable (directly or indirectly) from `main` or a
`preserved` routine, the routines in the group will not be considered
reachable.

    | define main routine
    | {
    | }
    | 
    | define other1 routine
    | {
    |   call other2
    | }
    | 
    | define other2 routine
    | {
    |   call other1
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [],
    =         "potentially-calls": [],
    =         "reachable": true
    =     },
    =     "other1": {
    =         "potentially-called-by": [
    =             "other2"
    =         ],
    =         "potentially-calls": [
    =             "other2"
    =         ]
    =     },
    =     "other2": {
    =         "potentially-called-by": [
    =             "other1"
    =         ],
    =         "potentially-calls": [
    =             "other1"
    =         ]
    =     }
    = }
