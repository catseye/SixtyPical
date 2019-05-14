SixtyPical Syntax
=================

This is a test suite, written in [Falderal][] format, for the syntax of
the Sixtypical language, disgregarding execution, static analysis, etc.

Note that these example programs are intended to be syntactically correct,
but not necessarily sensible programs.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Check syntax of SixtyPical program"

Rudimentary program.

    | define main routine {
    |     ld a, 0
    |     add a, 1
    | }
    = ok

Program with comments.

    | // Welcome to my program.
    | 
    | define main routine {
    |     ld a, 0
    |     add a, 1    // We are adding the thing.
    |     sub a, 1
    |     shl a
    |     shr a
    |     and a, 1
    |     or a, 1
    |     xor a, 1
    | }
    = ok

Hex literals.

    | define main routine {
    |     ld a, $ff
    |     add a, $01
    | }
    = ok

Syntax error.

    | define foo routine (
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

    | define chrout routine
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | define chrin routine
    |   outputs a
    |   trashes x
    |   @ 65487
    = ok

Trash.

    | define main routine {
    |     trash a
    |     trash n
    | }
    = ok

`nop`.

    | define main routine
    | {
    |     nop
    | }
    = ok

If with not

    | define foo routine {
    |     ld y, 0
    |     cmp y, 10
    |     if not z {
    |         inc y
    |         cmp y, 10
    |     }
    | }
    = ok

Repeat loop

    | define foo routine {
    |     ld y, 0
    |     repeat {
    |         inc y
    |         cmp y, 10
    |     } until z
    | }
    = ok

"While" loop

    | define foo routine inputs y {
    |     repeat {
    |         cmp y, 10
    |         if not z {
    |             inc y
    |         }
    |     } until z
    | }
    = ok

Repeat forever

    | define foo routine inputs y {
    |     repeat {
    |         inc y
    |     } forever
    | }
    = ok

Repeat with not

    | define foo routine inputs y {
    |     repeat {
    |         inc y
    |     } until not z
    | }
    = ok

Basic "open-faced for" loops, up and down.

    | byte table[256] tab
    | 
    | define foo routine trashes a, x, c, z, v {
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

Other blocks.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine trashes a, x, c, z, v {
    |     with interrupts off {
    |         save a, x, c {
    |             ld a, 0
    |         }
    |     }
    |     save a, x, c {
    |         ld a, 0
    |     }
    |     point ptr into tab {
    |         reset ptr 0
    |         ld a, [ptr] + y
    |     }
    | }
    = ok

User-defined memory addresses of different types.

    | byte byt
    | word wor
    | vector routine trashes a vec
    | byte table[2048] buf
    | pointer ptr
    | 
    | define main routine {
    | }
    = ok

Tables of different types and some operations on them.

    | byte table[256] many
    | word table[256] wmany
    | vector (routine trashes a) table[256] vmany
    | byte bval
    | word wval
    | 
    | define main routine {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     add a, many + x
    |     sub a, many + x
    |     cmp a, many + x
    |     and a, many + x
    |     or a, many + x
    |     xor a, many + x
    |     shl many + x
    |     shr many + x
    |     inc many + x
    |     dec many + x
    |     ld a, many + x
    |     st a, many + x
    |     copy wval, wmany + x
    |     copy wmany + x, wval
    | }
    = ok

Indexing with an offset in some tables.

    | byte table[256] many
    | word table[256] wmany
    | byte bval
    | word wval
    | 
    | define main routine {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     add a, many + 100 + x
    |     sub a, many + 100 + x
    |     cmp a, many + 100 + x
    |     and a, many + 100 + x
    |     or a, many + 100 + x
    |     xor a, many + 100 + x
    |     shl many + 100 + x
    |     shr many + 100 + x
    |     inc many + 100 + x
    |     dec many + 100 + x
    |     ld a, many + 100 + x
    |     st a, many + 100 + x
    |     copy wval, wmany + 100 + x
    |     copy wmany + 100 + x, wval
    | }
    = ok

The number of entries in a table must be
greater than 0 and less than or equal to 65536.

(In previous versions, a table could have at
most 256 entries.  They can now have more, however
the offset-access syntax can only access the
first 256.  To access more, a pointer is required.)

    | word table[512] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 9999, many + x
    | }
    = ok

    | byte table[65536] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 99, many + x
    | }
    = ok

    | byte table[65537] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 99, many + x
    | }
    ? SyntaxError

    | word table[0] many
    | 
    | define main routine
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
    | define main routine
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
    | define main routine {
    | }
    = ok

Can't have two typedefs with the same name.

    | typedef byte frank
    | typedef word frank
    | 
    | define main routine {
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
    | define main routine {
    |   ld a, lives
    | }
    = ok

Named constants can be used as offsets.

    | const lives 3
    | const w1 1000
    | 
    | byte table[w1] those
    | 
    | define main routine {
    |   ld y, 0
    |   ld a, those + lives + y
    | }
    = ok

Can't have two constants with the same name.

    | const w1 1000
    | const w1 word 0
    | 
    | define main routine {
    | }
    ? SyntaxError

Explicit memory address.

    | byte screen @ 1024
    | 
    | define main routine {
    |   ld a, 100
    |   st a, screen
    |   shl screen
    |   shr screen
    | }
    = ok

Initialized memory locations.

    | byte lives : 3
    | 
    | define main routine {
    |   ld a, lives
    |   st a, lives
    | }
    = ok

Cannot have both initial value and explicit address.

    | byte screen : 3 @ 1024
    | 
    | define main routine {
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
    | define main routine {
    | }
    = ok

Initialized byte table, initialized with ASCII string.

    | byte table[32] message : "WHAT DO YOU WANT TO DO NEXT?"
    | 
    | define main routine {
    | }
    = ok

Can't initialize anything but a byte table with a string.

    | word message : "OUCH! WHAT DO YOU DO?"
    | 
    | define main routine {
    | }
    ? SyntaxError

Initialized byte table, initialized with list of bytes.

    | byte table[8] charmap : 0, 255, 129, 192, 0, 1, 2, 4
    | 
    | define main routine {
    | }
    = ok

Can't access an undeclared memory location.

    | define main routine {
    |     ld a, 0
    |     st a, lives
    | }
    ? SyntaxError

Can't define two memory locations with the same name.

    | byte lives
    | byte lives
    | 
    | define main routine {
    |     ld a, 0
    |     st a, lives
    | }
    ? SyntaxError

Can't shadow the name of a register or a flag.

    | byte a
    | 
    | define main routine {
    | }
    ? SyntaxError

    | byte z
    | 
    | define main routine {
    | }
    ? SyntaxError

Can't call routine that hasn't been defined.

    | define main routine {
    |     ld x, 0
    |     ld y, 1
    |     call up
    |     call up
    | }
    ? SyntaxError

But you can call a routine that is yet to be defined, further on.

    | define main routine {
    |     ld x, 0
    |     ld y, 1
    |     call up
    |     call up
    | }
    | define up routine {
    |     ld a, 0
    | }
    = ok

Can't define two routines with the same name.

    | define main routine {
    |     inc x
    |     inc y
    | }
    | define main routine {
    |     ld x, 0
    |     ld y, 1
    | }
    ? SyntaxError

Declaring byte and word table memory location.

    | byte table[256] tab
    | 
    | define main routine {
    |     ld x, 0
    |     ld y, 0
    |     ld a, tab + x
    |     st a, tab + y
    | }
    = ok

    | word one
    | word table[256] many
    | 
    | define main routine {
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
    | define foo routine {
    |     ld a, 0
    | }
    | define main routine {
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
    | define main routine {
    | }
    ? SyntaxError

Constraints set may only contain labels.

    | vector routine
    |   inputs a
    |   outputs 200
    |   trashes a, x, z, n
    |   cinv @ 788
    | 
    | define foo routine {
    |     ld a, 0
    | }
    | define main routine {
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
    | define foo routine {
    |     ld a, 0
    | }
    | define main routine {
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
    | define main routine {
    |     with interrupts off {
    |         copy foo, cinv
    |     }
    |     call cinv
    | }
    | define foo routine {
    |     ld a, 0
    | }
    = ok

goto.

    | define foo routine {
    |     ld a, 0
    | }
    | define main routine {
    |     goto foo
    | }
    = ok

The label doesn't have to be defined yet at the point
in the program text where it is `goto`d.

    | define main routine {
    |     goto foo
    | }
    | define foo routine {
    |     ld a, 0
    | }
    = ok

Syntactically, you can `goto` a vector.

    | vector routine foo
    | 
    | define main routine {
    |     goto foo
    | }
    = ok

But you can't `goto` a label that never gets defined.

    | define main routine {
    |     goto foo
    | }
    ? SyntaxError

`goto` may only be the final instruction in a block.

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     goto bar
    |     ld x, 0
    | }
    ? Expected '}', but found 'ld'

Tables and pointers.

    | byte table[2048] buf
    | pointer ptr
    | pointer ptrb
    | byte foo
    | 
    | define main routine {
    |     point ptr into buf {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |         copy [ptr] + y, foo
    |         copy [ptr] + y, [ptrb] + y
    |     }
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
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok

    | typedef routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     routine_type
    | 
    | vector routine_type vec
    | 
    | define foo routine_type
    | {
    |   inc x
    | }
    | 
    | define main routine
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

Memory locations can be defined local to a routine.

    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t : 0
    |   local word w
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    | 
    | define main routine
    |   trashes a, x, z, n
    |   static byte t : 0
    |   local word w
    | {
    |   ld x, t
    |   call foo
    | }
    = ok

Local static memory locations must always be given an initial value.

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

Local static memory locations may not be given an address.

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t @ 1024
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError

Local dynamic memory locations may not be given an initial value.

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   local byte t : 10
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError

Local dynamic memory locations may be given an address.

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   local byte t @ 1024
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    = ok

Name of a local cannot shadow an existing global or local.

    | byte t
    | 
    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t : 10
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
    |   static byte t : 10
    |   static byte t : 20
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError

    | byte t
    | 
    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   local byte t
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
    |   local word w
    |   local word w
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    ? SyntaxError

Since the names of locals are lexically local to a routine, they cannot
appear in the inputs, outputs, trashes list of the routine.

    | define main routine
    |   inputs t
    |   static byte t : 0
    | {
    |   inc t
    | }
    ? SyntaxError

    | define main routine
    |   outputs t
    |   static byte t : 0
    | {
    |   inc t
    | }
    ? SyntaxError

    | define main routine
    |   trashes t
    |   static byte t : 0
    | {
    |   inc t
    | }
    ? SyntaxError

    | define main routine
    |   inputs t
    |   local byte t
    | {
    |   inc t
    | }
    ? SyntaxError

    | define main routine
    |   outputs t
    |   local byte t
    | {
    |   inc t
    | }
    ? SyntaxError

    | define main routine
    |   trashes t
    |   local byte t
    | {
    |   inc t
    | }
    ? SyntaxError
