SixtyPical Compilation
======================

This is a test suite, written in [Falderal][] format, for compiling
SixtyPical to 6502 machine code.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Compile SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --compile %(test-body-file) | fa-bin-to-hex"

    -> Tests for functionality "Compile SixtyPical program"

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

Call defined routine.

    | routine foo
    |   outputs a, x, y
    |   trashes z, n
    | {
    |   ld a, 0
    |   ld x, 0
    |   ld y, 0
    | }
    | 
    | routine main
    |   trashes a, x, y, z, n
    | {
    |     call foo
    | }
    = 00c02004c060a900a200a00060

Access a defined memory location.

    | byte foo
    | 
    | routine main
    |   trashes a, y, z, n, foo
    | {
    |     ld y, 0
    |     st y, foo
    |     ld a, foo
    | }
    = 00c0a0008c09c0ad09c060

Memory location with explicit address.

    | byte screen @ 1024
    | 
    | routine main
    |   trashes a, z, n, screen
    | {
    |   ld a, 100
    |   st a, screen
    | }
    = 00c0a9648d000460

Memory location with initial value.

    | byte lives : 3
    | 
    | routine main
    |   inputs lives
    |   trashes a, z, n
    | {
    |   ld a, lives
    | }
    = 00c0ad04c06003

Some instructions.

    | byte foo
    | 
    | routine main
    |   trashes a, x, y, z, n, c, v, foo
    | {
    |     ld a, 0
    |     ld x, 0
    |     ld y, 0
    |     st a, foo
    |     st x, foo
    |     st y, foo
    |     st on, c
    |     st off, c
    |     add a, 1
    |     add a, foo
    |     sub a, 1
    |     sub a, foo
    |     inc foo
    |     inc x
    |     inc y
    |     dec foo
    |     dec x
    |     dec y
    |     and a, 255
    |     and a, foo
    |     or a, 255
    |     or a, foo
    |     xor a, 255
    |     xor a, foo
    |     cmp a, 1
    |     cmp a, foo
    |     cmp x, 1
    |     cmp x, foo
    |     cmp y, 1
    |     cmp y, foo
    |     shl a
    |     shr a
    | }
    = 00c0a900a200a0008d46c08e46c08c46c0381869016d46c0e901ed46c0ee46c0e8c8ce46c0ca8829ff2d46c009ff0d46c049ff4d46c0c901cd46c0e001ec46c0c001cc46c02a6a60

Compiling `if`.

    | routine main
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     if z {
    |         ld y, 1
    |     } else {
    |         ld y, 2
    |     }
    | }
    = 00c0a900d005a0014c0bc0a00260

Compiling `if not`.

    | routine main
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     if not z {
    |         ld y, 1
    |     } else {
    |         ld y, 2
    |     }
    | }
    = 00c0a900f005a0014c0bc0a00260

Compiling `if` without `else`.

    | routine main
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     if z {
    |         ld y, 1
    |     }
    | }
    = 00c0a900d002a00160

Compiling `repeat`.

    | routine main
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         ld a, y
    |         inc y
    |         cmp y, 91
    |     } until z
    | }
    = 00c0a04198c8c05bd0fa60

Compiling `repeat until not`.

    | routine main
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         ld a, y
    |         inc y
    |         cmp y, 91
    |     } until not z
    | }
    = 00c0a04198c8c05bf0fa60

Compiling `repeat forever`.

    | routine main
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         inc y
    |     } forever
    | }
    = 00c0a041c84c02c060

Indexed access.

    | byte one
    | byte table many
    | 
    | routine main
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many + x
    |     ld a, many + x
    | }
    = 00c0a200a9009d0dc0bd0dc060

Copy byte to byte.

    | byte bar
    | byte baz
    | 
    | routine main
    |   inputs baz
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy baz, bar
    | }
    = 00c0ad09c08d07c060

Copy word to word.

    | word bar
    | word baz
    | 
    | routine main
    |   inputs baz
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy baz, bar
    | }
    = 00c0ad0fc08d0dc0ad10c08d0ec060

Copy literal word to word.

    | word bar
    | 
    | routine main
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy 65535, bar
    | }
    = 00c0a9ff8d0bc0a9ff8d0cc060

Copy vector to vector.

    | vector bar
    | vector baz
    | 
    | routine main
    |   inputs baz
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy baz, bar
    | }
    = 00c0ad0fc08d0dc0ad10c08d0ec060

Copy routine to vector, inside an `interrupts off` block.

    | vector bar
    | 
    | routine foo
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | routine main
    |   inputs foo
    |   outputs bar
    |   trashes a, n, z
    | {
    |   with interrupts off {
    |     copy foo, bar
    |   }
    | }
    = 00c078a90d8d0fc0a9c08d10c05860e860

Indirect call.

    | vector foo outputs x trashes z, n
    | 
    | routine bar outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | routine main inputs bar outputs x, foo trashes a, z, n {
    |     copy bar, foo
    |     call foo
    | }
    = 00c0a90e8d14c0a9c08d15c02011c060a2c8606c14c0

goto.

    | routine bar outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | routine main outputs x trashes a, z, n {
    |     ld y, 200
    |     goto bar
    | }
    = 00c0a0c84c06c060a2c860

### word operations

Adding a constant word to a word memory location.

    | word score
    | routine main
    |   inputs score
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    = 00c018ad12c069cf8d12c0ad13c069078d13c060

Adding a word memory location to another word memory location.

    | word score
    | word delta
    | routine main
    |   inputs score, delta
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add score, delta
    | }
    = 00c018ad14c06d16c08d14c0ad15c06d17c08d15c060

### Buffers and Pointers

Load address into pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | 
    | routine main
    |   inputs buf
    |   outputs buf, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    | }
    = 00c0a000a90b85fea9c085ff60

Write literal through a pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | 
    | routine main
    |   inputs buf
    |   outputs buf, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy 123, [ptr] + y
    | }
    = 00c0a000a90f85fea9c085ffa97b91fe60

Write stored value through a pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | byte foo
    | 
    | routine main
    |   inputs foo, buf
    |   outputs y, buf
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy foo, [ptr] + y
    | }
    = 00c0a000a91085fea9c085ffad12c091fe60

Read through a pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | byte foo
    | 
    | routine main
    |   inputs buf
    |   outputs y, foo
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy [ptr] + y, foo
    | }
    = 00c0a000a91085fea9c085ffb1fe8d12c060
