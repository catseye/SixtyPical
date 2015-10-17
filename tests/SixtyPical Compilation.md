Sixtypical Compilation
======================

This is a test suite, written in [Falderal][] format, for compiling
Sixtypical to 6502 machine code.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Compile Sixtypical program" is implemented by
    -> shell command "bin/sixtypical --compile %(test-body-file) | fa-bin-to-hex"

    -> Tests for functionality "Compile Sixtypical program"

Null program.

    | routine main
    | {
    | }
    = 00c060

Rudimentary program.

    | routine main
    |   inputs a
    |   outputs a
    |   trashes c, z, n, v
    | {
    |     st off, c
    |     add a, 4
    | }
    = 00c018690460

Call extern.

    | routine chrout
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | routine main
    |   inputs a
    |   trashes a, z, n
    | {
    |     ld a, 65
    |     call chrout
    | }
    = 00c0a94120d2ff60
