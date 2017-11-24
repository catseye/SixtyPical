SixtyPical Syntax
=================

This is a test suite, written in [Falderal][] format, for the syntax of
the Sixtypical language, disgregarding execution, static analysis, etc.

Note that these example programs are intended to be syntactically correct,
but not necessarily sensible programs.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Check syntax of SixtyPical program" is implemented by
    -> shell command "bin/sixtypical %(test-body-file) && echo ok"

    -> Tests for functionality "Check syntax of SixtyPical program"

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

User-defined memory addresses of different types.

    | byte byt
    | word wor
    | vector vec
    | byte table tab
    | 
    | routine main {
    | }
    = ok

Buffer and pointer types.

    | byte byt
    | word wor
    | vector vec
    | byte table tab
    | buffer[2048] buf
    | pointer ptr
    | 
    | routine main {
    | }
    = ok

Explicit memory address.

    | byte screen @ 1024
    | 
    | routine main {
    |   ld a, 100
    |   st a, screen
    | }
    = ok

Initialized memory locations.

    | byte lives : 3
    | 
    | routine main {
    |   ld a, lives
    |   st a, lives
    | }
    = ok

Cannot have both initial value and explicit address.

    | byte screen : 3 @ 1024
    | 
    | routine main {
    |   ld a, lives
    |   st a, lives
    | }
    ? SyntaxError

User-defined locations of other types.

    | byte table screen @ 1024
    | word r1
    | word r2 @ 60000
    | word r3 : 2000
    | 
    | routine main {
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

Can't call routine that hasn't been defined.

    | routine main {
    |     ld x, 0
    |     ld y, 1
    |     call up
    |     call up
    | }
    ? SyntaxError

And you can't call a non-routine.

    | byte up
    | 
    | routine main {
    |     ld x, 0
    |     ld y, 1
    |     call up
    | }
    ? SyntaxError

    | routine main {
    |     ld x, 0
    |     ld y, 1
    |     call x
    | }
    ? SyntaxError

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

Declaring and calling a vector.

    | vector cinv
    |   inputs a
    |   outputs x
    |   trashes a, x, z, n
    |   @ 788
    | 
    | routine foo {
    |     ld a, 0
    | }
    | routine main {
    |     with interrupts off {
    |         copy foo, cinv
    |     }
    |     call cinv
    | }
    = ok

Only vectors can be decorated with constraints like that.

    | byte cinv
    |   inputs a
    |   outputs x
    |   trashes a, x, z, n
    |   @ 788
    | 
    | routine main {
    | }
    ? SyntaxError

goto.

    | routine foo {
    |     ld a, 0
    | }
    | routine main {
    |     goto foo
    | }
    = ok

    | vector foo
    | 
    | routine main {
    |     goto foo
    | }
    = ok

    | routine main {
    |     goto foo
    | }
    ? SyntaxError

    | byte foo
    | 
    | routine main {
    |     goto foo
    | }
    ? SyntaxError
