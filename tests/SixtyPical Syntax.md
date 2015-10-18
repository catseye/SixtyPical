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

Program with comments.

    | // Welcome to my program.
    | 
    | routine main {
    |     ld a, 0
    |     add a, 1    // We are adding the thing.
    | }
    = ok

Hex literals.

    | routine main {
    |     ld a, $ff
    |     add a, $01
    | }
    = ok

Syntax error.

    | routine foo (
    |     ld a, 0
    |     add a, 1
    | )
    ? SyntaxError

Another syntax error.

    | byte glee
    | {
    |     ld a, 0
    |     add a, 1
    | }
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

If with not

    | routine foo {
    |     ld y, 0
    |     cmp y, 10
    |     if not z {
    |         inc y
    |         cmp y, 10
    |     }
    | }
    = ok

Repeat loop

    | routine foo {
    |     ld y, 0
    |     repeat {
    |         inc y
    |         cmp y, 10
    |     } until z
    | }
    = ok

"While" loop

    | routine foo inputs y {
    |     repeat {
    |         cmp y, 10
    |         if not z {
    |             inc y
    |         }
    |     } until z
    | }
    = ok

Repeat forever

    | routine foo inputs y {
    |     repeat {
    |         inc y
    |     } forever
    | }
    = ok

Repeat with not

    | routine foo inputs y {
    |     repeat {
    |         inc y
    |     } until not z
    | }
    = ok

Extern memory locations

    | byte screen @ 1024
    | 
    | routine main {
    |   ld a, 100
    |   st a, screen
    | }
    = ok

Can't access an undeclared memory location.

    | routine main {
    |     ld a, 0
    |     st a, lives
    | }
    ? SyntaxError

Can't define two memory locations with the same name.

    | byte lives
    | byte lives
    | 
    | routine main {
    |     ld a, 0
    |     st a, lives
    | }
    ? SyntaxError

Can't shadow the name of a register or a flag.

    | byte a
    | 
    | routine main {
    | }
    ? SyntaxError

    | byte z
    | 
    | routine main {
    | }
    ? SyntaxError

> Can't call routine that hasn;t been defined.
> 
>     | routine main {
>     |     ld x, 0
>     |     ld y, 1
>     |     call up
>     |     call up
>     | }
>     ? SyntaxError

Can't define two routines with the same name.

    | routine main {
    |     inc x
    |     inc y
    | }
    | routine main {
    |     ld x, 0
    |     ld y, 1
    | }
    ? SyntaxError

Declaring a byte table memory location.

    | byte table tab
    | 
    | routine main {
    |     ld x, 0
    |     ld y, 0
    |     ld a, tab + x
    |     st a, tab + y
    | }
    = ok

Declaring a vector.

    | vector cinv
    | 
    | routine foo {
    |     ld a, 0
    | }
    | routine main {
    |     copy foo, cinv
    | }
    = ok
