SixtyPical Callgraph
====================

This is a test suite, written in [Falderal][] format, for the ability of
a SixtyPical analyzer to construct a callgraph of which routines call which
other routines, and its ability to discover which routines will never be
called.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Dump callgraph info for SixtyPical program"

The `main` routine is always called.

    | define main routine
    | {
    | }
    = [
    =     [
    =         "main"
    =     ]
    = ]
