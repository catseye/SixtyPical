SixtyPical Callgraph
====================

This is a test suite, written in [Falderal][] format, for the ability of
a SixtyPical analyzer to construct a callgraph of which routines call which
other routines, and its ability to discover which routines will never be
called.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Dump callgraph info for SixtyPical program"

The `main` routine is always called.  The thing that it will
be called by is the system, but the callgraph analyzer will
simply consider it to be "marked as called".

    | define main routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [
    =             "*marked*"
    =         ],
    =         "potentially-calls": []
    =     }
    = }

If a routine is called by another routine, this fact will be noted.

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
    =         "potentially-called-by": [
    =             "*marked*"
    =         ],
    =         "potentially-calls": [
    =             "other"
    =         ]
    =     },
    =     "other": {
    =         "potentially-called-by": [
    =             "main"
    =         ],
    =         "potentially-calls": []
    =     }
    = }

If a routine is not called by another routine, and it is not `main`
and it is not explicitly marked as preserved, this absence will be
noted, and a compiler or linker will be permitted to omit it from
the final executable.

    | define main routine
    | {
    | }
    | 
    | define other routine
    | {
    | }
    = {
    =     "main": {
    =         "potentially-called-by": [
    =             "*marked*"
    =         ],
    =         "potentially-calls": []
    =     },
    =     "other": {
    =         "potentially-called-by": [],
    =         "potentially-calls": []
    =     }
    = }

If two routines potentially call each other, this will be noted,
even if nothing else potentially calls either of those routines.
This may change in the future.

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
    =         "potentially-called-by": [
    =             "*marked*"
    =         ],
    =         "potentially-calls": []
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
