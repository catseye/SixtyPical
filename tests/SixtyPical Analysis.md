SixtyPical Analysis
===================

<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

This is a test suite, written in [Falderal][] format, for the SixtyPical
static analysis rules.

This file mostly contains tests for operations.
For rudiments and storage, see [SixtyPical Storage](SixtyPical%20Storage.md).
For control flow, see [SixtyPical Control Flow](SixtyPical%20Control%20Flow.md).

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Analyze SixtyPical program"

### add ###

Can't `add` from or to a memory location that isn't initialized.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, 0
    | }
    = ok

    | byte lives
    | define main routine
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, lives
    | }
    ? UnmeaningfulReadError: lives

    | byte lives
    | define main routine
    |   inputs lives
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, lives
    | }
    ? UnmeaningfulReadError: a

Can't `add` to a memory location that isn't writeable.

    | define main routine
    |   inputs a
    |   trashes c
    | {
    |     st off, c
    |     add a, 0
    | }
    ? ForbiddenWriteError: a

You can `add` a byte constant to a byte memory location.

    | byte lives
    | define main routine
    |   inputs a, lives
    |   outputs lives
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add lives, 3
    | }
    = ok

`add`ing a byte constant to a byte memory location trashes `a`.

    | byte lives
    | define main routine
    |   inputs a, lives
    |   outputs a, lives
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add lives, 3
    | }
    ? UnmeaningfulOutputError: a

You can `add` a byte memory location to another byte memory location.
This trashes `a`.

    | byte lives
    | byte extra
    | define main routine
    |   inputs a, lives, extra
    |   outputs lives
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add lives, extra
    | }
    = ok

    | byte lives
    | byte extra
    | define main routine
    |   inputs a, lives, extra
    |   outputs a, lives
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add lives, extra
    | }
    ? UnmeaningfulOutputError: a

You can `add` a word constant to a word memory location.

    | word score
    | define main routine
    |   inputs a, score
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    = ok

`add`ing a word constant to a word memory location trashes `a`.

    | word score
    | define main routine
    |   inputs a, score
    |   outputs score, a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    ? UnmeaningfulOutputError: a

To be sure, `add`ing a word constant to a word memory location trashes `a`.

    | word score
    | define main routine
    |   inputs score
    |   outputs score
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    ? ForbiddenWriteError: a

You can `add` a word memory location to another word memory location.

    | word score
    | word delta
    | define main routine
    |   inputs score, delta
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add score, delta
    | }
    = ok

`add`ing a word memory location to a word memory location trashes `a`.

    | word score
    | word delta
    | define main routine
    |   inputs score, delta
    |   outputs score
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add score, delta
    | }
    ? ForbiddenWriteError: a

You can `add` a word memory location, or a constant, to a pointer.

    | pointer ptr
    | word delta
    | define main routine
    |   inputs ptr, delta
    |   outputs ptr
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add ptr, delta
    |     add ptr, word 1
    | }
    = ok

`add`ing a word memory location, or a constant, to a pointer, trashes `a`.

    | pointer ptr
    | word delta
    | define main routine
    |   inputs ptr, delta
    |   outputs ptr
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add ptr, delta
    |     add ptr, word 1
    | }
    ? ForbiddenWriteError: a

### sub ###

Can't `sub` from or to a memory location that isn't initialized.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, 0
    | }
    = ok

    | byte lives
    | define main routine
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, lives
    | }
    ? UnmeaningfulReadError: lives

    | byte lives
    | define main routine
    |   inputs lives
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, lives
    | }
    ? UnmeaningfulReadError: a

Can't `sub` to a memory location that isn't writeable.

    | define main routine
    |   inputs a
    |   trashes c
    | {
    |     st off, c
    |     sub a, 0
    | }
    ? ForbiddenWriteError: a

You can `sub` a byte constant from a byte memory location.

    | byte lives
    | define main routine
    |   inputs a, lives
    |   outputs lives
    |   trashes a, c, z, v, n
    | {
    |     st on, c
    |     sub lives, 3
    | }
    = ok

`sub`ing a byte constant from a byte memory location trashes `a`.

    | byte lives
    | define main routine
    |   inputs a, lives
    |   outputs a, lives
    |   trashes c, z, v, n
    | {
    |     st on, c
    |     sub lives, 3
    | }
    ? UnmeaningfulOutputError: a

You can `sub` a byte memory location from another byte memory location.
This trashes `a`.

    | byte lives
    | byte extra
    | define main routine
    |   inputs a, lives, extra
    |   outputs lives
    |   trashes a, c, z, v, n
    | {
    |     st on, c
    |     sub lives, extra
    | }
    = ok

    | byte lives
    | byte extra
    | define main routine
    |   inputs a, lives, extra
    |   outputs a, lives
    |   trashes c, z, v, n
    | {
    |     st on, c
    |     sub lives, extra
    | }
    ? UnmeaningfulOutputError: a

You can `sub` a word constant from a word memory location.

    | word score
    | define main routine
    |   inputs a, score
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st on, c
    |     sub score, 1999
    | }
    = ok

`sub`ing a word constant from a word memory location trashes `a`.

    | word score
    | define main routine
    |   inputs a, score
    |   outputs score, a
    |   trashes c, z, v, n
    | {
    |     st on, c
    |     sub score, 1999
    | }
    ? UnmeaningfulOutputError: a

You can `sub` a word memory location from another word memory location.

    | word score
    | word delta
    | define main routine
    |   inputs score, delta
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     sub score, delta
    | }
    = ok

`sub`ing a word memory location from a word memory location trashes `a`.

    | word score
    | word delta
    | define main routine
    |   inputs score, delta
    |   outputs score
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub score, delta
    | }
    ? ForbiddenWriteError: a

### inc ###

Location must be initialized and writeable.

    | define main routine
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    ? UnmeaningfulReadError: x

    | define main routine
    |   inputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    ? ForbiddenWriteError: x

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    = ok

Can't `inc` a `word` type.

    | word foo
    | 
    | define main routine
    |   inputs foo
    |   outputs foo
    |   trashes z, n
    | {
    |     inc foo
    | }
    ? TypeMismatchError: foo

### dec ###

Location must be initialized and writeable.

    | define main routine
    |   outputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    ? UnmeaningfulReadError: x

    | define main routine
    |   inputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    ? ForbiddenWriteError: x

    | define main routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    = ok

Can't `dec` a `word` type.

    | word foo
    | 
    | define main routine
    |   inputs foo
    |   outputs foo
    |   trashes z, n
    | {
    |     dec foo
    | }
    ? TypeMismatchError: foo

### cmp ###

Some rudimentary tests for `cmp`.

    | define main routine
    |   inputs a
    |   trashes z, c, n
    | {
    |     cmp a, 4
    | }
    = ok

    | define main routine
    |   inputs a
    |   trashes z, n
    | {
    |     cmp a, 4
    | }
    ? ForbiddenWriteError: c

    | define main routine
    |   trashes z, c, n
    | {
    |     cmp a, 4
    | }
    ? UnmeaningfulReadError: a

`cmp` can work on words. In this case, it trashes `a`.

    | word za
    | word zb
    | 
    | define main routine
    |   inputs za, zb
    |   trashes a, z, c, n
    | {
    |     cmp za, zb
    | }
    = ok

    | word za
    | word zb
    | 
    | define main routine
    |   inputs za, zb
    |   trashes a, z, n
    | {
    |     cmp za, zb
    | }
    ? ForbiddenWriteError: c

    | word za
    | word zb
    | 
    | define main routine
    |   inputs za, zb
    |   trashes z, c, n
    | {
    |     cmp za, zb
    | }
    ? ForbiddenWriteError: a

    | word za
    | word zb
    | 
    | define main routine
    |   inputs za
    |   trashes z, c, n
    | {
    |     cmp za, zb
    | }
    ? UnmeaningfulReadError: zb

`cmp` can compare against a literal word.

    | word za
    | 
    | define main routine
    |   inputs za
    |   trashes a, z, c, n
    | {
    |     cmp za, 4000
    | }
    = ok

    | word za
    | 
    | define main routine
    |   inputs za
    |   trashes z, c, n
    | {
    |     cmp za, 4000
    | }
    ? ForbiddenWriteError: a

### and ###

Some rudimentary tests for `and`.

    | define main routine
    |   inputs a
    |   outputs a, z, n
    | {
    |     and a, 4
    | }
    = ok

    | define main routine
    |   inputs a
    |   trashes z, n
    | {
    |     and a, 4
    | }
    ? ForbiddenWriteError: a

    | define main routine
    |   trashes z, n
    | {
    |     and a, 4
    | }
    ? UnmeaningfulReadError: a

### or ###

Some rudimentary tests for `or`.

    | define main routine
    |   inputs a
    |   outputs a, z, n
    | {
    |     or a, 4
    | }
    = ok

    | define main routine
    |   inputs a
    |   trashes z, n
    | {
    |     or a, 4
    | }
    ? ForbiddenWriteError: a

    | define main routine
    |   trashes z, n
    | {
    |     or a, 4
    | }
    ? UnmeaningfulReadError: a

### xor ###

Some rudimentary tests for `xor`.

    | define main routine
    |   inputs a
    |   outputs a, z, n
    | {
    |     xor a, 4
    | }
    = ok

    | define main routine
    |   inputs a
    |   trashes z, n
    | {
    |     xor a, 4
    | }
    ? ForbiddenWriteError: a

    | define main routine
    |   trashes z, n
    | {
    |     xor a, 4
    | }
    ? UnmeaningfulReadError: a

### shl ###

Some rudimentary tests for `shl`.

    | byte foo
    | define main routine
    |   inputs foo, a, c
    |   outputs foo, a, c, z, n
    | {
    |     shl a
    |     shl foo
    | }
    = ok

    | define main routine
    |   inputs a, c
    |   outputs c, z, n
    | {
    |     shl a
    | }
    ? ForbiddenWriteError: a

    | define main routine
    |   inputs a
    |   outputs a, c, z, n
    | {
    |     shl a
    | }
    ? UnmeaningfulReadError: c

### shr ###

Some rudimentary tests for `shr`.

    | byte foo
    | define main routine
    |   inputs foo, a, c
    |   outputs foo, a, c, z, n
    | {
    |     shr a
    |     shr foo
    | }
    = ok

    | define main routine
    |   inputs a, c
    |   outputs c, z, n
    | {
    |     shr a
    | }
    ? ForbiddenWriteError: a

    | define main routine
    |   inputs a
    |   outputs a, c, z, n
    | {
    |     shr a
    | }
    ? UnmeaningfulReadError: c

### nop ###

Some rudimentary tests for `nop`.

    | define main routine
    | {
    |     nop
    | }
    = ok

### with interrupts ###

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     bar
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | define main routine
    |   outputs bar
    |   trashes a, n, z
    | {
    |   with interrupts off {
    |     copy foo, bar
    |   }
    | }
    = ok

A `goto` cannot appear within a `with interrupts` block.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     bar
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | define other routine
    |   trashes bar, a, n, z
    | {
    |    ld a, 0
    | }
    | 
    | define main routine
    |   trashes bar, a, n, z
    | {
    |   with interrupts off {
    |     copy foo, bar
    |     goto other
    |   }
    | }
    ? IllegalJumpError

A `call` cannot appear within a `with interrupts` block.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     bar
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | define other routine
    |   trashes bar, a, n, z
    | {
    |    ld a, 0
    | }
    | 
    | define main routine
    |   trashes bar, a, n, z
    | {
    |   with interrupts off {
    |     copy foo, bar
    |     call other
    |   }
    | }
    ? IllegalJumpError

A `with interrupts` block cannot appear within a `with interrupts` block.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     bar
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | define main routine
    |   trashes bar, a, n, z
    | {
    |   with interrupts off {
    |     copy foo, bar
    |     with interrupts off {
    |       copy foo, bar
    |     }
    |   }
    | }
    ? IllegalJumpError

### typedef ###

A typedef is a more-readable alias for a type.  "Alias" means
that types have structural equivalence, not name equivalence.

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

The new style routine definitions support typedefs.

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
