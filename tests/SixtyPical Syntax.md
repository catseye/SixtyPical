Sixtypical Execution
====================

This is a test suite, written in [Falderal][] format, for the syntax of
the Sixtypical language, disgregarding execution, static analysis, etc.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Check syntax of Sixtypical program" is implemented by
    -> shell command "bin/sixtypical %(test-body-file) && echo ok"

    -> Tests for functionality "Check syntax of Sixtypical program"

Rudimentary program.

    | routine main {
    |     ld a, 0
    |     add a, 1
    | }
    = ok

Syntax error

    | routine foo (
    |     ld a, 0
    |     add a, 1
    | )
    ? SyntaxError

Extern routines

    | routine chrout
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | routine chrin
    |   outputs a
    |   trashes x
    |   @ 65487
    = ok
