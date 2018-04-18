SixtyPical Syntax
=================

This is a test suite, written in [Falderal][] format, for the syntax of
the Sixtypical language, disgregarding execution, static analysis, etc.

Note that these example programs are intended to be syntactically correct,
but not necessarily sensible programs.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Check syntax of SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --parse-only --traceback %(test-body-file) && echo ok"

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

Trash.

    | routine main {
    |     trash a
    |     trash n
    | }
    = ok

`nop`.

    | routine main
    | {
    |     nop
    | }
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

Basic "open-faced for" loops, up and down.

    | byte table[256] tab
    | 
    | routine foo trashes a, x, c, z, v {
    |     ld x, 0
    |     for x up to 15 {
    |         ld a, tab + x
    |     }
    |     ld x, 15
    |     for x down to 0 {
    |         ld a, tab + x
    |     }
    | }
    = ok

User-defined memory addresses of different types.

    | byte byt
    | word wor
    | vector routine trashes a vec
    | buffer[2048] buf
    | pointer ptr
    | 
    | routine main {
    | }
    = ok

Tables of different types.

    | byte table[256] tab
    | word table[256] wtab
    | vector (routine trashes a) table[256] vtab
    | 
    | routine main {
    | }
    = ok

The number of entries in a table must be
greater than 0 and less than or equal to 256.

    | word table[512] many
    | 
    | routine main
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 9999, many + x
    | }
    ? SyntaxError

    | word table[0] many
    | 
    | routine main
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 9999, many + x
    | }
    ? SyntaxError

    | word table[48] many
    | 
    | routine main
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 9999, many + x
    | }
    = ok

Typedefs of different types.

    | typedef byte octet
    | typedef octet table[256] twokay
    | typedef routine trashes a game_routine
    | vector game_routine start_game
    | 
    | routine main {
    | }
    = ok

Can't have two typedefs with the same name.

    | typedef byte frank
    | typedef word frank
    | 
    | routine main {
    | }
    ? SyntaxError

Constants.

    | const lives 3
    | const days lives
    | const w1 1000
    | const w2 word 0
    | 
    | typedef byte table[days] them
    | 
    | byte lark: lives
    | 
    | routine main {
    |   ld a, lives
    | }
    = ok

Can't have two constants with the same name.

    | const w1 1000
    | const w1 word 0
    | 
    | routine main {
    | }
    ? SyntaxError

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

    | byte table[256] screen @ 1024
    | word r1
    | word r2 @ 60000
    | word r3 : 2000
    | 
    | routine main {
    | }
    = ok

Initialized byte table, initialized with ASCII string.

    | byte table[32] message : "WHAT DO YOU WANT TO DO NEXT?"
    | 
    | routine main {
    | }
    = ok

Can't initialize anything but a byte table with a string.

    | word message : "WHAT DO YOU WANT TO DO NEXT?"
    | 
    | routine main {
    | }
    ? SyntaxError

Initialized byte table, initialized with list of bytes.

    | byte table[8] charmap : 0, 255, 129, 192, 0, 1, 2, 4
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

But you can call a routine that is yet to be defined, further on.

    | routine main {
    |     ld x, 0
    |     ld y, 1
    |     call up
    |     call up
    | }
    | routine up {
    |     ld a, 0
    | }
    = ok

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

Declaring byte and word table memory location.

    | byte table[256] tab
    | 
    | routine main {
    |     ld x, 0
    |     ld y, 0
    |     ld a, tab + x
    |     st a, tab + y
    | }
    = ok

    | word one
    | word table[256] many
    | 
    | routine main {
    |     ld x, 0
    |     copy one, many + x
    |     copy word 0, many + x
    |     copy many + x, one
    | }
    = ok

Declaring and calling a vector.

    | vector routine
    |   inputs a
    |   outputs x
    |   trashes a, x, z, n
    |   cinv @ 788
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

Constraints set may only contain labels.

    | vector routine
    |   inputs a
    |   outputs 200
    |   trashes a, x, z, n
    |   cinv @ 788
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
    ? SyntaxError

A vector can name itself in its inputs, outputs, and trashes.

    | vector routine
    |   inputs cinv, a
    |   outputs cinv, x
    |   trashes a, x, z, n
    |   cinv @ 788
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

A routine can be copied into a vector before the routine appears in the program.
This is known as a "forward reference".  You are only allowed to make forward
references in the source of a `copy` instruction.

    | vector routine
    |   inputs cinv, a
    |   outputs cinv, x
    |   trashes a, x, z, n
    |   cinv @ 788
    | routine main {
    |     with interrupts off {
    |         copy foo, cinv
    |     }
    |     call cinv
    | }
    | routine foo {
    |     ld a, 0
    | }
    = ok

goto.

    | routine foo {
    |     ld a, 0
    | }
    | routine main {
    |     goto foo
    | }
    = ok

    | routine main {
    |     goto foo
    | }
    | routine foo {
    |     ld a, 0
    | }
    = ok

    | vector routine foo
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

Buffers and pointers.

    | buffer[2048] buf
    | pointer ptr
    | pointer ptrb
    | byte foo
    | 
    | routine main {
    |     copy ^buf, ptr
    |     copy 123, [ptr] + y
    |     copy [ptr] + y, foo
    |     copy [ptr] + y, [ptrb] + y
    | }
    = ok

Routines can be defined in a new style.

    | typedef routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     routine_type
    | 
    | vector routine_type vec
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |   inc x
    | }
    | 
    | routine main
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok

Only routines can be defined in the new style.

    | define foo byte table[256]
    | 
    | routine main
    |   trashes a, z, n
    | {
    |     ld a, 0
    | }
    ? SyntaxError

Memory locations can be defined static to a routine.

    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t : 0
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    | 
    | define main routine
    |   trashes a, x, z, n
    |   static byte t : 0
    | {
    |   ld x, t
    |   call foo
    | }
    = ok

Static memory locations must always be given an initial value.

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError

Name of a static cannot shadow an existing global or static.

    | byte t
    | 
    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t
    |   static byte t
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError
