Sixtypical Execution
====================

This is a test suite, written in [Falderal][] format, for the dynamic
execution behaviour of the Sixtypical language, disgregarding static analysis.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Execute Sixtypical program" is implemented by
    -> shell command "bin/sixtypical --execute %(test-body-file)"

    -> Tests for functionality "Execute Sixtypical program"

Rudimentary program.

    | routine main {
    |     ld a, 0
    |     add a, 1
    | }
    = a: 1
    = c: 0
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Program accesses a memory location.

    | byte lives
    | 
    | routine main {
    |     ld a, 0
    |     st a, lives
    |     ld x, lives
    |     add x, 1
    |     st x, lives
    | }
    = a: 0
    = c: 0
    = lives: 1
    = n: 0
    = v: 0
    = x: 1
    = y: 0
    = z: 0

Program accesses a memory location with initial value.

    | byte lives : 3
    | 
    | routine main {
    |     ld a, lives
    | }
    = a: 3
    = c: 0
    = lives: 3
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Add honours carry.

    | routine main {
    |     ld a, 255
    |     st on, c
    |     add a, 0
    |   }
    = a: 0
    = c: 1
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 1

    | routine main {
    |     ld a, $ff
    |     st off, c
    |     add a, 1
    |   }
    = a: 0
    = c: 1
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 1

Subtract honours carry.

    | routine main {
    |     ld a, 0
    |     st on, c
    |     sub a, 0
    |   }
    = a: 255
    = c: 1
    = n: 1
    = v: 0
    = x: 0
    = y: 0
    = z: 0

    | routine main {
    |     ld a, 0
    |     st off, c
    |     sub a, 1
    | }
    = a: 255
    = c: 1
    = n: 1
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Inc and dec do not honour carry, but do set n and z.

    | routine main {
    |     ld x, 254
    |     st on, c
    |     inc x
    | }
    = a: 0
    = c: 1
    = n: 1
    = v: 0
    = x: 255
    = y: 0
    = z: 0

    | routine main {
    |     ld y, 1
    |     st on, c
    |     dec y
    | }
    = a: 0
    = c: 1
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 1

Compare affects, but does not use, carry.

    | routine main {
    |     ld a, 1
    |     st on, c
    |     cmp a, 1
    | }
    = a: 1
    = c: 0
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 1

    | routine main {
    |     ld a, 1
    |     st off, c
    |     cmp a, 5
    | }
    = a: 1
    = c: 1
    = n: 1
    = v: 0
    = x: 0
    = y: 0
    = z: 0

AND.

    | routine main {
    |     ld a, 15
    |     and a, 18
    |   }
    = a: 2
    = c: 0
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

OR.

    | routine main {
    |     ld a, 34
    |     or a, 18
    |   }
    = a: 50
    = c: 0
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

XOR.

    | routine main {
    |     ld a, 34
    |     xor a, 18
    |   }
    = a: 48
    = c: 0
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Shift left.

    | routine main {
    |     ld a, 129
    |     st off, c
    |     shl a
    |   }
    = a: 2
    = c: 1
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

    | routine main {
    |     ld a, 0
    |     st on, c
    |     shl a
    |   }
    = a: 1
    = c: 0
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Shift right.

    | routine main {
    |     ld a, 129
    |     st off, c
    |     shr a
    |   }
    = a: 64
    = c: 1
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

    | routine main {
    |     ld a, 0
    |     st on, c
    |     shr a
    |   }
    = a: 128
    = c: 0
    = n: 1
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Call routine.

    | routine up {
    |     inc x
    |     inc y
    | }
    | routine main {
    |     ld x, 0
    |     ld y, 1
    |     call up
    |     call up
    | }
    = a: 0
    = c: 0
    = n: 0
    = v: 0
    = x: 2
    = y: 3
    = z: 0

If.

    | routine main {
    |     ld x, 40
    |     cmp x, 40
    |     if z {
    |         ld a, 1
    |     } else {
    |         ld a, 8
    |     }
    |     ld x, 2
    |   }
    = a: 1
    = c: 0
    = n: 0
    = v: 0
    = x: 2
    = y: 0
    = z: 0

    | routine main {
    |     ld x, 39
    |     cmp x, 40
    |     if z {
    |         ld a, 1
    |     } else {
    |         ld a, 8
    |     }
    |     ld x, 2
    |   }
    = a: 8
    = c: 1
    = n: 0
    = v: 0
    = x: 2
    = y: 0
    = z: 0

If without else.

    | routine main {
    |     ld x, 39
    |     cmp x, 40
    |     if z {
    |         ld a, 1
    |     }
    |     ld x, 2
    |   }
    = a: 0
    = c: 1
    = n: 0
    = v: 0
    = x: 2
    = y: 0
    = z: 0

`not` inverts the sense of the test.

    | routine main {
    |     ld x, 40
    |     cmp x, 40
    |     if not z {
    |         ld a, 1
    |     } else {
    |         ld a, 8
    |     }
    |     ld x, 2
    |   }
    = a: 8
    = c: 0
    = n: 0
    = v: 0
    = x: 2
    = y: 0
    = z: 0

    | routine main {
    |     ld x, 39
    |     cmp x, 40
    |     if not z {
    |         ld a, 1
    |     }
    |     ld x, 2
    |   }
    = a: 1
    = c: 1
    = n: 0
    = v: 0
    = x: 2
    = y: 0
    = z: 0

Repeat loop.

    | routine main {
    |     ld x, 0
    |     ld y, 15
    |     repeat {
    |         inc x
    |         inc y
    |         cmp x, 10
    |     } until z
    | }
    = a: 0
    = c: 0
    = n: 0
    = v: 0
    = x: 10
    = y: 25
    = z: 1

Copy instruction.  Note that the state of a, z, and n are not defined
after copy executes.

    | routine main {
    |     ld x, 5
    |     copy x, y
    | }
    = a: 0
    = c: 0
    = n: 0
    = v: 0
    = x: 5
    = y: 5
    = z: 0

Copy word to word.

    | word foo : 2000
    | word bar
    | 
    | routine main {
    |     copy foo, bar
    | }
    = a: 0
    = bar: 2000
    = c: 0
    = foo: 2000
    = n: 0
    = v: 0
    = x: 0
    = y: 0
    = z: 0

Load and store low byte, high byte of word.

    | word foo : 511
    | 
    | routine main {
    |     ld x, <foo
    |     ld y, >foo
    |     ld a, 2
    |     st a, <foo
    |     ld a, 1
    |     st a, >foo
    | }
    = a: 1
    = c: 0
    = foo: 258
    = n: 0
    = v: 0
    = x: 255
    = y: 1
    = z: 0

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
    = a: 0
    = c: 0
    = n: 1
    = v: 0
    = x: 200
    = y: 0
    = z: 0

goto.

    | routine bar outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | routine main outputs x trashes a, z, n {
    |     ld y, 200
    |     goto bar
    | }
    = a: 0
    = c: 0
    = n: 1
    = v: 0
    = x: 200
    = y: 200
    = z: 0
