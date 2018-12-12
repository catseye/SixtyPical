SixtyPical Analysis
===================

This is a test suite, written in [Falderal][] format, for the SixtyPical
static analysis rules.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Analyze SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --analyze-only --traceback %(test-body-file) && echo ok"

    -> Tests for functionality "Analyze SixtyPical program"

### Rudiments ###

Routines must declare their inputs, outputs, and memory locations they trash.

    | define up routine
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, 1
    | }
    = ok

Routines may not declare a memory location to be both an output and trashed.

    | define main routine
    |   outputs a
    |   trashes a
    | {
    |     ld a, 0
    | }
    ? InconsistentConstraintsError: a

If a routine declares it outputs a location, that location should be initialized.

    | define main routine
    |   outputs a, x, z, n
    | {
    |     ld x, 0
    | }
    ? UnmeaningfulOutputError: a

    | define main routine
    |   inputs a
    |   outputs a
    | {
    | }
    = ok

If a routine declares it outputs a location, that location may or may not have
been initialized.  Trashing is mainly a signal to the caller.

    | define main routine
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | define main routine
    |   trashes x, z, n
    | {
    | }
    = ok

If a routine modifies a location, it needs to either output it or trash it.

    | define main routine
    | {
    |     ld x, 0
    | }
    ? ForbiddenWriteError: x

    | define main routine
    |   outputs x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | define main routine
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

This is true regardless of whether it's an input or not.

    | define main routine
    |   inputs x
    | {
    |     ld x, 0
    | }
    ? ForbiddenWriteError: x

    | define main routine
    |   inputs x
    |   outputs x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | define main routine
    |   inputs x
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

If a routine trashes a location, this must be declared.

    | define foo routine
    |   trashes x
    | {
    |     trash x
    | }
    = ok

    | define foo routine
    | {
    |     trash x
    | }
    ? ForbiddenWriteError: x

    | define foo routine
    |   outputs x
    | {
    |     trash x
    | }
    ? UnmeaningfulOutputError: x

If a routine causes a location to be trashed, this must be declared in the caller.

    | define trash_x routine
    |   trashes x, z, n
    | {
    |   ld x, 0
    | }
    | 
    | define foo routine
    |   trashes x, z, n
    | {
    |     call trash_x
    | }
    = ok

    | define trash_x routine
    |   trashes x, z, n
    | {
    |   ld x, 0
    | }
    | 
    | define foo routine
    |   trashes z, n
    | {
    |     call trash_x
    | }
    ? ForbiddenWriteError: x

    | define trash_x routine
    |   trashes x, z, n
    | {
    |   ld x, 0
    | }
    | 
    | define foo routine
    |   outputs x
    |   trashes z, n
    | {
    |     call trash_x
    | }
    ? UnmeaningfulOutputError: x (in foo, line 12)

If a routine reads or writes a user-define memory location, it needs to declare that too.

    | byte b1 @ 60000
    | byte b2 : 3
    | word w1 @ 60001
    | word w2 : 2000
    | 
    | define main routine
    |   inputs b1, w1
    |   outputs b2, w2
    |   trashes a, z, n
    | {
    |   ld a, b1
    |   st a, b2
    |   copy w1, w2
    | }
    = ok

### call ###

You can't call a non-routine.

    | byte up
    | 
    | define main routine outputs x, y trashes z, n {
    |     ld x, 0
    |     ld y, 1
    |     call up
    | }
    ? TypeMismatchError: up

    | define main routine outputs x, y trashes z, n {
    |     ld x, 0
    |     ld y, 1
    |     call x
    | }
    ? TypeMismatchError: x

Nor can you goto a non-routine.

    | byte foo
    | 
    | define main routine {
    |     goto foo
    | }
    ? TypeMismatchError: foo

### ld ###

Can't `ld` from a memory location that isn't initialized.

    | define main routine
    |   inputs a, x
    |   trashes a, z, n
    | {
    |     ld a, x
    | }
    = ok

    | define main routine
    |   inputs a
    |   trashes a
    | {
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x

Can't `ld` to a memory location that doesn't appear in (outputs ∪ trashes).

    | define main routine
    |   trashes a, z, n
    | {
    |     ld a, 0
    | }
    = ok

    | define main routine
    |   outputs a
    |   trashes z, n
    | {
    |     ld a, 0
    | }
    = ok

    | define main routine
    |   outputs z, n
    |   trashes a
    | {
    |     ld a, 0
    | }
    = ok

    | define main routine
    |   trashes z, n
    | {
    |     ld a, 0
    | }
    ? ForbiddenWriteError: a

    | define main routine
    |   trashes a, n
    | {
    |     ld a, 0
    | }
    ? ForbiddenWriteError: z

Can't `ld` a `word` type.

    | word foo
    | 
    | define main routine
    |   inputs foo
    |   trashes a, n, z
    | {
    |     ld a, foo
    | }
    ? TypeMismatchError: foo and a

### st ###

Can't `st` from a memory location that isn't initialized.

    | byte lives
    | define main routine
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    = ok

    | byte lives
    | define main routine
    |   trashes x, lives
    | {
    |     st x, lives
    | }
    ? UnmeaningfulReadError: x

Can't `st` to a memory location that doesn't appear in (outputs ∪ trashes).

    | byte lives
    | define main routine
    |   trashes lives
    | {
    |     st 0, lives
    | }
    = ok

    | byte lives
    | define main routine
    |   outputs lives
    | {
    |     st 0, lives
    | }
    = ok

    | byte lives
    | define main routine
    |   inputs lives
    | {
    |     st 0, lives
    | }
    ? ForbiddenWriteError: lives

Can't `st` a `word` type.

    | word foo
    | 
    | define main routine
    |   outputs foo
    |   trashes a, n, z
    | {
    |     ld a, 0
    |     st a, foo
    | }
    ? TypeMismatchError

### tables ###

Storing to a table, you must use an index.

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   outputs one
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, one
    | }
    = ok

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many
    | }
    ? TypeMismatchError

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   outputs one
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, one + x
    | }
    ? TypeMismatchError

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many + x
    | }
    = ok

The index must be initialized.

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld a, 0
    |     st a, many + x
    | }
    ? UnmeaningfulReadError: x

Reading from a table, you must use an index.

    | byte one
    | 
    | define main routine
    |   outputs one
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     st x, one
    |     ld a, one
    | }
    = ok

    | byte one
    | 
    | define main routine
    |   outputs one
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     st x, one
    |     ld a, one + x
    | }
    ? TypeMismatchError

    | byte table[256] many
    | 
    | define main routine
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many + x
    |     ld a, many
    | }
    ? TypeMismatchError

    | byte table[256] many
    | 
    | define main routine
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many + x
    |     ld a, many + x
    | }
    = ok

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, many + x
    | }
    = ok

The index must be initialized.

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld a, many + x
    | }
    ? UnmeaningfulReadError: x

There are other operations you can do on tables. (1/3)

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z, v
    | {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     add a, many + x
    |     sub a, many + x
    |     cmp a, many + x
    | }
    = ok

There are other operations you can do on tables. (2/3)

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     and a, many + x
    |     or a, many + x
    |     xor a, many + x
    | }
    = ok

There are other operations you can do on tables. (3/3)

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     shl many + x
    |     shr many + x
    |     inc many + x
    |     dec many + x
    | }
    = ok

Copying to and from a word table.

    | word one
    | word table[256] many
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy one, many + x
    |     copy many + x, one
    | }
    = ok

    | word one
    | word table[256] many
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy one, many
    | }
    ? TypeMismatchError

    | word one
    | word table[256] many
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy one + x, many
    | }
    ? TypeMismatchError

You can also copy a literal word to a word table.
(Even if the table has fewer than 256 entries.)

    | word table[32] many
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

#### tables: range checking ####

It is a static analysis error if it cannot be proven that a read or write
to a table falls within the defined size of that table.

(If a table has 256 entries, then there is never a problem, because a byte
cannot index any entry outside of 0..255.)

A SixtyPical implementation must be able to prove that the index is inside
the range of the table in various ways.  The simplest is to show that a
constant value falls inside or outside the range of the table.

    | byte table[32] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 31
    |     ld a, many + x
    |     st a, many + x
    | }
    = ok

    | byte table[32] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 32
    |     ld a, many + x
    | }
    ? RangeExceededError

    | byte table[32] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 32
    |     ld a, 0
    |     st a, many + x
    | }
    ? RangeExceededError

This applies to `copy` as well.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     ld x, 31
    |     copy one, many + x
    |     copy many + x, one
    | }
    = ok

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     ld x, 32
    |     copy many + x, one
    | }
    ? RangeExceededError

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     ld x, 32
    |     copy one, many + x
    | }
    ? RangeExceededError

`AND`'ing a register with a value ensures the range of the
register will not exceed the range of the value.  This can
be used to "clip" the range of an index so that it fits in
a table.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs a, many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     and a, 31
    |     ld x, a
    |     copy one, many + x
    |     copy many + x, one
    | }
    = ok

Test for "clipping", but not enough.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs a, many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     and a, 63
    |     ld x, a
    |     copy one, many + x
    |     copy many + x, one
    | }
    ? RangeExceededError

If you alter the value after "clipping" it, the range can
no longer be guaranteed.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs a, many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     and a, 31
    |     ld x, a
    |     inc x
    |     copy one, many + x
    |     copy many + x, one
    | }
    ? RangeExceededError

When the range of a location is known, incrementing or
decrementing that location's value will shift the known
range.  It will not invalidate it unless the known range
is at the limits of the possible ranges for the type.

    | vector routine
    |   trashes a, z, n
    |     print
    | 
    | vector (routine
    |   trashes a, z, n)
    |     table[32] vectors
    | 
    | define main routine
    |   inputs vectors, print
    |   outputs vectors
    |   trashes print, a, x, z, n, c
    | {
    |     ld x, 0
    |     inc x
    |     copy print, vectors + x
    | }
    = ok

    | vector routine
    |   trashes a, z, n
    |     print
    | 
    | vector (routine
    |   trashes a, z, n)
    |     table[32] vectors
    | 
    | define main routine
    |   inputs vectors, print
    |   outputs vectors
    |   trashes print, a, x, z, n, c
    | {
    |     ld x, 32
    |     dec x
    |     copy print, vectors + x
    | }
    = ok

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
    |   outputs lives
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add lives, 3
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
    |   outputs lives
    |   trashes c, z, v, n
    | {
    |     st on, c
    |     sub lives, 3
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

### call ###

When calling a routine, all of the locations it lists as inputs must be
initialized.

    | byte lives
    | 
    | define foo routine
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | define main routine
    | {
    |     call foo
    | }
    ? UnmeaningfulReadError: x

Note that if you call a routine that trashes a location, you also trash it.

    | byte lives
    | 
    | define foo routine
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | define main routine
    |   outputs x, z, n
    | {
    |     ld x, 0
    |     call foo
    | }
    ? ForbiddenWriteError: lives

    | byte lives
    | 
    | define foo routine
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | define main routine
    |   outputs x, z, n
    |   trashes lives
    | {
    |     ld x, 0
    |     call foo
    | }
    = ok

You can't output a value that the thing you called trashed.

    | byte lives
    | 
    | define foo routine
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | define main routine
    |   outputs x, z, n, lives
    | {
    |     ld x, 0
    |     call foo
    | }
    ? UnmeaningfulOutputError: lives

...unless you write to it yourself afterwards.

    | byte lives
    | 
    | define foo routine
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | define main routine
    |   outputs x, z, n, lives
    | {
    |     ld x, 0
    |     call foo
    |     st x, lives
    | }
    = ok

If a routine declares outputs, they are initialized in the caller after
calling it.

    | define foo routine
    |   outputs x, z, n
    | {
    |     ld x, 0
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     call foo
    |     ld a, x
    | }
    = ok

    | define foo routine
    | {
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x
    | {
    |     call foo
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x

If a routine trashes locations, they are uninitialized in the caller after
calling it.

    | define foo routine
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | define foo routine
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     call foo
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x

Calling an extern is just the same as calling a defined routine with the
same constraints.

    | define chrout routine
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     ld a, 65
    |     call chrout
    | }
    = ok

    | define chrout routine
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     call chrout
    | }
    ? UnmeaningfulReadError: a

    | define chrout routine
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | define main routine
    |   trashes a, x, z, n
    | {
    |     ld a, 65
    |     call chrout
    |     ld x, a
    | }
    ? UnmeaningfulReadError: a

### trash ###

Trash does nothing except indicate that we do not care about the value anymore.

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n
    | {
    |     st a, x
    |     ld a, 0
    |     trash a
    | }
    = ok

    | define foo routine
    |   inputs a
    |   outputs a, x
    |   trashes z, n
    | {
    |     st a, x
    |     ld a, 0
    |     trash a
    | }
    ? UnmeaningfulOutputError: a

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n
    | {
    |     st a, x
    |     trash a
    |     st a, x
    | }
    ? UnmeaningfulReadError: a

### if ###

Both blocks of an `if` are analyzed.

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     cmp a, 42
    |     if z {
    |         ld x, 7
    |     } else {
    |         ld x, 23
    |     }
    | }
    = ok

If a location is initialized in one block, is must be initialized in the other as well.

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     cmp a, 42
    |     if z {
    |         ld x, 7
    |     } else {
    |         ld a, 23
    |     }
    | }
    ? InconsistentInitializationError: x

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     cmp a, 42
    |     if z {
    |         ld a, 6
    |     } else {
    |         ld x, 7
    |     }
    | }
    ? InconsistentInitializationError: x

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     cmp a, 42
    |     if not z {
    |         ld a, 6
    |     } else {
    |         ld x, 7
    |     }
    | }
    ? InconsistentInitializationError: x

However, this only pertains to initialization.  If a value is already
initialized, either because it was set previous to the `if`, or is an
input to the routine, and it is initialized in one branch, it need not
be initialized in the other.

    | define foo routine
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     ld x, 0
    |     ld a, 0
    |     cmp a, 42
    |     if z {
    |         ld x, 7
    |     } else {
    |         ld a, 23
    |     }
    | }
    = ok

    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     ld a, 0
    |     cmp a, 42
    |     if z {
    |         ld x, 7
    |     } else {
    |         ld a, 23
    |     }
    | }
    = ok

An `if` with a single block is analyzed as if it had an empty `else` block.

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     cmp a, 42
    |     if z {
    |         ld x, 7
    |     }
    | }
    ? InconsistentInitializationError: x

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     ld x, 0
    |     cmp a, 42
    |     if z {
    |         ld x, 7
    |     }
    | }
    = ok

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     ld x, 0
    |     cmp a, 42
    |     if not z {
    |         ld x, 7
    |     }
    | }
    = ok

The cardinal rule for trashes in an `if` is the "union rule": if one branch
trashes {`a`} and the other branch trashes {`b`} then the whole `if` statement
trashes {`a`, `b`}.

    | define foo routine
    |   inputs a, x, z
    |   trashes a, x
    | {
    |     if z {
    |         trash a
    |     } else {
    |         trash x
    |     }
    | }
    = ok

    | define foo routine
    |   inputs a, x, z
    |   trashes a
    | {
    |     if z {
    |         trash a
    |     } else {
    |         trash x
    |     }
    | }
    ? ForbiddenWriteError: x (in foo, line 10)

    | define foo routine
    |   inputs a, x, z
    |   trashes x
    | {
    |     if z {
    |         trash a
    |     } else {
    |         trash x
    |     }
    | }
    ? ForbiddenWriteError: a (in foo, line 10)

### repeat ###

Repeat loop.

    | define main routine
    |   outputs x, y, n, z, c
    | {
    |     ld x, 0
    |     ld y, 15
    |     repeat {
    |         inc x
    |         inc y
    |         cmp x, 10
    |     } until z
    | }
    = ok

You can initialize something inside the loop that was uninitialized outside.

    | define main routine
    |   outputs x, y, n, z, c
    | {
    |     ld x, 0
    |     repeat {
    |         ld y, 15
    |         inc x
    |         cmp x, 10
    |     } until z
    | }
    = ok

But you can't UNinitialize something at the end of the loop that you need
initialized at the start.

    | define foo routine
    |   trashes y
    | {
    | }
    | 
    | define main routine
    |   outputs x, y, n, z, c
    | {
    |     ld x, 0
    |     ld y, 15
    |     repeat {
    |         inc x
    |         inc y
    |         call foo
    |         cmp x, 10
    |     } until z
    | }
    ? UnmeaningfulReadError: y

And if you trash the test expression (i.e. `z` in the below) inside the loop,
this is an error too.

    | word one : 0
    | word two : 0
    | 
    | define main routine
    |   inputs one, two
    |   outputs two
    |   trashes a, z, n
    | {
    |     repeat {
    |         copy one, two
    |     } until z
    | }
    ? UnmeaningfulReadError: z

The body of `repeat forever` can be empty.

    | define main routine
    | {
    |     repeat {
    |     } forever
    | }
    = ok

While `repeat` is most often used with `z`, it can also be used with `n`.

    | define main routine
    |   outputs y, n, z
    | {
    |     ld y, 15
    |     repeat {
    |         dec y
    |     } until n
    | }
    = ok

### for ###

Basic "open-faced for" loop.  We'll start with the "upto" variant.

#### upward-counting variant

Even though we do not give the starting value in the "for" construct,
we know the exact range the loop variable takes on.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 0
    |     for x up to 15 {
    |         ld a, tab + x
    |     }
    | }
    = ok

    | byte table[15] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 0
    |     for x up to 15 {
    |         ld a, tab + x
    |     }
    | }
    ? RangeExceededError

You need to initialize the loop variable before the loop.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     for x up to 15 {
    |         ld a, 0
    |     }
    | }
    ? UnmeaningfulReadError

Because routines currently do not include range constraints,
the loop variable may not be useful as an input (the location
is assumed to have the maximum range.)

    | byte table[16] tab
    | 
    | define foo routine
    |   inputs tab, x
    |   trashes a, x, c, z, v, n {
    |     for x up to 15 {
    |         ld a, 0
    |     }
    | }
    ? RangeExceededError

You cannot modify the loop variable in a "for" loop.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 0
    |     for x up to 15 {
    |         ld x, 0
    |     }
    | }
    ? ForbiddenWriteError

This includes nesting a loop on the same variable.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 0
    |     for x up to 8 {
    |         for x up to 15 {
    |             ld a, 0
    |         }
    |     }
    | }
    ? ForbiddenWriteError

But nesting with two different variables is okay.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, y, c, z, v, n {
    |     ld x, 0
    |     for x up to 8 {
    |         ld a, x
    |         ld y, a
    |         for y up to 15 {
    |             ld a, 0
    |         }
    |     }
    | }
    = ok

Inside the inner loop, the outer variable is still not writeable.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, y, c, z, v, n {
    |     ld x, 0
    |     for x up to 8 {
    |         ld a, x
    |         ld y, a
    |         for y up to 15 {
    |             ld x, 0
    |         }
    |     }
    | }
    ? ForbiddenWriteError

If the range isn't known to be smaller than the final value, you can't go up to it.

    | byte table[32] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 16
    |     for x up to 15 {
    |         ld a, tab + x
    |     }
    | }
    ? RangeExceededError

You can initialize something inside the loop that was uninitialized outside.

    | define main routine
    |   outputs x, y, n, z
    |   trashes c
    | {
    |     ld x, 0
    |     for x up to 15 {
    |         ld y, 15
    |     }
    | }
    = ok

But you can't UNinitialize something at the end of the loop that you need
initialized at the start of that loop.

    | define foo routine
    |   trashes y
    | {
    | }
    | 
    | define main routine
    |   outputs x, y, n, z
    |   trashes c
    | {
    |     ld x, 0
    |     ld y, 15
    |     for x up to 15 {
    |         inc y
    |         call foo
    |     }
    | }
    ? UnmeaningfulReadError: y

The "for" loop does not preserve the `z` or `n` registers.

    | define foo routine trashes x {
    |     ld x, 0
    |     for x up to 15 {
    |     }
    | }
    ? ForbiddenWriteError

But it does preserve the other registers, such as `c`.

    | define foo routine trashes x, z, n {
    |     ld x, 0
    |     for x up to 15 {
    |     }
    | }
    = ok

In fact it does not strictly trash `z` and `n`, as they are
always set to known values after the loop.  TODO: document
what these known values are!

    | define foo routine outputs z, n trashes x {
    |     ld x, 0
    |     for x up to 15 {
    |     }
    | }
    = ok

#### downward-counting variant

In a "for" loop (downward-counting variant), we know the exact range the loop variable takes on.

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 15
    |     for x down to 0 {
    |         ld a, tab + x
    |     }
    | }
    = ok

    | byte table[15] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 15
    |     for x down to 0 {
    |         ld a, tab + x
    |     }
    | }
    ? RangeExceededError

You need to initialize the loop variable before a "for" loop  (downward variant).

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     for x down to 15 {
    |         ld a, 0
    |     }
    | }
    ? UnmeaningfulReadError

You cannot modify the loop variable in a "for" loop (downward variant).

    | byte table[16] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 15
    |     for x down to 0 {
    |         ld x, 0
    |     }
    | }
    ? ForbiddenWriteError

If the range isn't known to be larger than the final value, you can't go down to it.

    | byte table[32] tab
    | 
    | define foo routine inputs tab trashes a, x, c, z, v, n {
    |     ld x, 0
    |     for x down to 0 {
    |         ld a, tab + x
    |     }
    | }
    ? RangeExceededError

The "for" loop does not preserve the `z` or `n` registers.

    | define foo routine trashes x {
    |     ld x, 15
    |     for x down to 0 {
    |     }
    | }
    ? ForbiddenWriteError

But it does preserve the other registers, such as `c`.

    | define foo routine trashes x, z, n {
    |     ld x, 15
    |     for x down to 0 {
    |     }
    | }
    = ok

In fact it does not strictly trash `z` and `n`, as they are
always set to known values after the loop.  TODO: document
what these known values are!

    | define foo routine outputs z, n trashes x {
    |     ld x, 15
    |     for x down to 0 {
    |     }
    | }
    = ok

### save ###

Basic neutral test, where the `save` makes no difference.

    | define main routine
    |   inputs a, x
    |   outputs a, x
    |   trashes z, n
    | {
    |     ld a, 1
    |     save x {
    |         ld a, 2
    |     }
    |     ld a, 3
    | }
    = ok

Saving any location (other than `a`) will trash `a`.

    | define main routine
    |   inputs a, x
    |   outputs a, x
    |   trashes z, n
    | {
    |     ld a, 1
    |     save x {
    |         ld a, 2
    |     }
    | }
    ? UnmeaningfulOutputError

Saving `a` does not trash anything.

    | define main routine
    |   inputs a, x
    |   outputs a, x
    |   trashes z, n
    | {
    |     ld x, 1
    |     save a {
    |         ld x, 2
    |     }
    |     ld x, 3
    | }
    = ok

A defined value that has been saved can be trashed inside the block.
It will continue to be defined outside the block.

    | define main routine
    |   outputs x, y
    |   trashes a, z, n
    | {
    |     ld x, 0
    |     save x {
    |         ld y, 0
    |         trash x
    |     }
    | }
    = ok

A trashed value that has been saved can be used inside the block.
It will continue to be trashed outside the block.

(Note, both x and a are unmeaningful in this test.)

    | define main routine
    |   inputs a
    |   outputs a, x
    |   trashes z, n
    | {
    |     ld x, 0
    |     trash x
    |     save x {
    |         ld a, 0
    |         ld x, 1
    |     }
    | }
    ? UnmeaningfulOutputError

The known range of a value will be preserved outside the block as well.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs a, many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     and a, 31
    |     ld x, a
    |     save x {
    |         ld x, 255
    |     }
    |     copy one, many + x
    |     copy many + x, one
    | }
    = ok

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs a, many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     and a, 63
    |     ld x, a
    |     save x {
    |         ld x, 1
    |     }
    |     copy one, many + x
    |     copy many + x, one
    | }
    ? RangeExceededError

The known properties of a value are preserved inside the block, too.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs a, many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     and a, 31
    |     ld x, a
    |     save x {
    |         copy one, many + x
    |         copy many + x, one
    |     }
    |     copy one, many + x
    |     copy many + x, one
    | }
    = ok

A value which is not output from the routine, is preserved by the
routine; and can appear in a `save` exactly because a `save` preserves it.

    | define main routine
    |   outputs y
    |   trashes a, z, n
    | {
    |     save x {
    |         ld y, 0
    |         ld x, 1
    |     }
    | }
    = ok

Because saving anything except `a` trashes `a`, a common idiom is to save `a`
first in a nested series of `save`s.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes z, n
    | {
    |     save a {
    |         save x {
    |             ld a, 0
    |             ld x, 1
    |         }
    |     }
    | }
    = ok

There is a shortcut syntax for a nested series of `save`s.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes z, n
    | {
    |     save a, x {
    |         ld a, 0
    |         ld x, 1
    |     }
    | }
    = ok

`a` is only preserved if it is the outermost thing `save`d.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes z, n
    | {
    |     save x, a {
    |         ld a, 0
    |         ld x, 1
    |     }
    | }
    ? UnmeaningfulOutputError: a

Not just registers, but also user-defined locations can be saved.

    | byte foo
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     save foo {
    |         st 5, foo
    |     }
    | }
    = ok

But only if they are bytes.

    | word foo
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     save foo {
    |         copy 555, foo
    |     }
    | }
    ? TypeMismatchError

    | byte table[16] tab
    | 
    | define main routine
    |   trashes a, y, z, n
    | {
    |     save tab {
    |         ld y, 0
    |         st 5, tab + y
    |     }
    | }
    ? TypeMismatchError

A `goto` cannot appear within a `save` block.

    | define other routine
    |   trashes a, z, n
    | {
    |     ld a, 0
    | }
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     ld a, 1
    |     save x {
    |         ld x, 2
    |         goto other
    |     }
    | }
    ? IllegalJumpError

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

### copy ###

Can't `copy` from a memory location that isn't initialized.

    | byte lives
    | define main routine
    |   inputs x
    |   outputs lives
    |   trashes a, z, n
    | {
    |     copy x, lives
    | }
    = ok

    | byte lives
    | define main routine
    |   outputs lives
    |   trashes x, a, z, n
    | {
    |     copy x, lives
    | }
    ? UnmeaningfulReadError: x

Can't `copy` to a memory location that doesn't appear in (outputs ∪ trashes).

    | byte lives
    | define main routine
    |   trashes lives, a, z, n
    | {
    |     copy 0, lives
    | }
    = ok

    | byte lives
    | define main routine
    |   outputs lives
    |   trashes a, z, n
    | {
    |     copy 0, lives
    | }
    = ok

    | byte lives
    | define main routine
    |   inputs lives
    |   trashes a, z, n
    | {
    |     copy 0, lives
    | }
    ? ForbiddenWriteError: lives

a, z, and n are trashed, and must be declared as such.

(Note, both n and z are forbidden writes in this test.)

    | byte lives
    | define main routine
    |   outputs lives
    | {
    |     copy 0, lives
    | }
    ? ForbiddenWriteError

a, z, and n are trashed, and must not be declared as outputs.

(Note, both n and a are unmeaningful outputs in this test.)

    | byte lives
    | define main routine
    |   outputs lives, a, z, n
    | {
    |     copy 0, lives
    | }
    ? UnmeaningfulOutputError

Unless of course you subsequently initialize them.

    | byte lives
    | define main routine
    |   outputs lives, a, z, n
    | {
    |     copy 0, lives
    |     ld a, 0
    | }
    = ok

Can `copy` from a `byte` to a `byte`.

    | byte source : 0
    | byte dest
    | 
    | define main routine
    |   inputs source
    |   outputs dest
    |   trashes a, z, n
    | {
    |     copy source, dest
    | }
    = ok

The understanding is that, because `copy` trashes `a`, `a` cannot be used
as the destination of a `copy`.

    | byte source : 0
    | byte dest
    | 
    | define main routine
    |   inputs source
    |   outputs dest
    |   trashes a, z, n
    | {
    |     copy source, a
    | }
    ? ForbiddenWriteError

Can `copy` from a `word` to a `word`.

    | word source : 0
    | word dest
    | 
    | define main routine
    |   inputs source
    |   outputs dest
    |   trashes a, z, n
    | {
    |     copy source, dest
    | }
    = ok

Can't `copy` from a `byte` to a `word`.

    | byte source : 0
    | word dest
    | 
    | define main routine
    |   inputs source
    |   outputs dest
    |   trashes a, z, n
    | {
    |     copy source, dest
    | }
    ? TypeMismatchError

Can't `copy` from a `word` to a `byte`.

    | word source : 0
    | byte dest
    | 
    | define main routine
    |   inputs source
    |   outputs dest
    |   trashes a, z, n
    | {
    |     copy source, dest
    | }
    ? TypeMismatchError

### Buffers and pointers ###

Note that `^buf` is a constant value, so it by itself does not require `buf` to be
listed in any input/output sets.

However, if the code reads from it through a pointer, it *should* be in `inputs`.

Likewise, if the code writes to it through a pointer, it *should* be in `outputs`.

Of course, unless you write to *all* the bytes in a buffer, some of those bytes
might not be meaningful.  So how meaningful is this check?

This is an open problem.

For now, convention says: if it is being read, list it in `inputs`, and if it is
being modified, list it in both `inputs` and `outputs`.

Write literal through a pointer.

    | buffer[2048] buf
    | pointer ptr
    | 
    | define main routine
    |   inputs buf
    |   outputs y, buf
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy 123, [ptr] + y
    | }
    = ok

It does use `y`.

    | buffer[2048] buf
    | pointer ptr
    | 
    | define main routine
    |   inputs buf
    |   outputs buf
    |   trashes a, z, n, ptr
    | {
    |     copy ^buf, ptr
    |     copy 123, [ptr] + y
    | }
    ? UnmeaningfulReadError

Write stored value through a pointer.

    | buffer[2048] buf
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs foo, buf
    |   outputs y, buf
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy foo, [ptr] + y
    | }
    = ok

Read through a pointer.

    | buffer[2048] buf
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs buf
    |   outputs foo
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy [ptr] + y, foo
    | }
    = ok

Read and write through two pointers.

    | buffer[2048] buf
    | pointer ptra
    | pointer ptrb
    | 
    | define main routine
    |   inputs buf
    |   outputs buf
    |   trashes a, y, z, n, ptra, ptrb
    | {
    |     ld y, 0
    |     copy ^buf, ptra
    |     copy ^buf, ptrb
    |     copy [ptra] + y, [ptrb] + y
    | }
    = ok

Read through a pointer to the `a` register.  Note that this is done with `ld`,
not `copy`.

    | buffer[2048] buf
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs buf
    |   outputs a
    |   trashes y, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     ld a, [ptr] + y
    | }
    = ok

Write the `a` register through a pointer.  Note that this is done with `st`,
not `copy`.

    | buffer[2048] buf
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs buf
    |   outputs buf
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     ld a, 255
    |     st a, [ptr] + y
    | }
    = ok

### routines ###

Routines are constants.  You need not, and in fact cannot, specify a constant
as an input to, an output of, or as a trashed value of a routine.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
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
    |   inputs foo
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? ConstantConstraintError: foo

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
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
    |   outputs vec, foo
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? ConstantConstraintError: foo

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
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
    |   trashes a, z, n, foo
    | {
    |     copy foo, vec
    | }
    ? ConstantConstraintError: foo

#### routine-vector type compatibility

You can copy the address of a routine into a vector, if that vector type
is at least as "wide" as the type of the routine.  More specifically,

- the vector must take _at least_ the inputs that the routine takes
- the vector must produce _at least_ the outputs that the routine produces
- the vector must trash _at least_ what the routine trashes

If the vector and the routine have the very same signature, that's not an error.

    | vector routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok

If the vector takes an input that the routine doesn't take, that's not an error.
(The interface requires that a parameter be specified before calling, but the
implementation doesn't actually read it.)

    | vector routine
    |   inputs x, y, a
    |   outputs x, y
    |   trashes z, n
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok

If the vector fails to take an input that the routine takes, that's an error.

    | vector routine
    |   inputs x
    |   outputs x, y
    |   trashes z, n
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? IncompatibleConstraintsError

If the vector produces an output that the routine doesn't produce, that's not an error.
(The interface claims the result of calling the routine is defined, but the implementation
actually preserves it instead of changing it; the caller can still treat it as a defined
output.)

    | vector routine
    |   inputs x, y
    |   outputs x, y, a
    |   trashes z, n
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok

If the vector fails to produce an output that the routine produces, that's an error.

    | vector routine
    |   inputs x, y
    |   outputs x
    |   trashes z, n
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? IncompatibleConstraintsError

If the vector fails to trash something the routine trashes, that's an error.

    | vector routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? IncompatibleConstraintsError

If the vector trashes something the routine doesn't trash, that's not an error.
(The implementation preserves something the interface doesn't guarantee is
preserved.  The caller gets no guarantee that it's preserved.  It actually is,
but it doesn't know that.)

    | vector routine
    |   inputs x, y
    |   outputs x, y
    |   trashes a, z, n
    |     vec
    | 
    | define foo routine
    |   inputs x, y
    |   outputs x, y
    |   trashes z, n
    | {
    |   inc x
    |   inc y
    | }
    | 
    | define main routine
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok

#### other properties of routines

Routines are read-only.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
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
    |     copy vec, foo
    | }
    ? TypeMismatchError

Indirect call.

    | vector routine
    |   outputs x trashes z, n
    |     foo
    | 
    | define bar routine outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | define main routine outputs x, foo trashes a, z, n {
    |     copy bar, foo
    |     call foo
    | }
    = ok

Calling the vector does indeed trash the things the vector says it does.

    | vector routine trashes x, z, n foo
    | 
    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine outputs x, foo trashes z, n {
    |     ld x, 0
    |     copy bar, foo
    |     call foo
    | }
    ? UnmeaningfulOutputError: x

For now at least, you cannot have a `goto` inside a `repeat` loop.

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     repeat {
    |         inc x
    |         goto bar
    |     } until z
    | }
    ? IllegalJumpError

`goto`, as a matter of syntax, can only appear at the end
of a block; but it need not be the final instruction in a
routine.

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     goto bar
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     }
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     }
    |     goto bar
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     }
    |     ld x, 0
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 0
    |         goto bar
    |     }
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 0
    |     }
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 0
    |     }
    |     ld x, 0
    | }
    = ok

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 0
    |     }
    |     goto bar
    | }
    = ok

Even though `goto` can only appear at the end of a block,
you can still wind up with dead code; the analysis detects
this.

    | define bar routine trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 0
    |         goto bar
    |     }
    |     ld x, 100
    | }
    ? TerminatedContextError

It is important that the type context at every
`goto` is compatible with the type context at the end of
the routine.

    | define bar routine
    |   inputs x
    |   trashes x, z, n
    | {
    |     ld x, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 0
    |     }
    |     ld x, 1
    | }
    = ok

Here, we try to trash `x` before `goto`ing a routine that inputs `x`.

    | define bar routine
    |   inputs x
    |   trashes x, z, n
    | {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 0
    |     if z {
    |         trash x
    |         goto bar
    |     } else {
    |         trash x
    |     }
    |     ld a, 1
    | }
    ? UnmeaningfulReadError: x

Here, we declare that main outputs `a`, but we `goto` a routine that does not output `a`.

    | define bar routine
    |   inputs x
    |   trashes x, z, n
    | {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 2
    |     }
    |     ld a, 1
    | }
    ? UnmeaningfulOutputError: a

Here, we declare that main outputs a, and we goto a routine that outputs a so that's OK.

    | define bar routine
    |   inputs x
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 200
    |     ld a, 1
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     } else {
    |         ld x, 2
    |     }
    |     ld a, 1
    | }
    = ok

Here, we declare that main outputs `a`, and we `goto` two routines, and they both output `a`.

    | define bar0 routine
    |   inputs x
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld a, x
    | }
    | 
    | define bar1 routine
    |   inputs x
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld a, 200
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar0
    |     } else {
    |         ld x, 2
    |         goto bar1
    |     }
    | }
    = ok

Here is like just above, but one routine doesn't output `a`.

    | define bar0 routine
    |   inputs x
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld a, x
    | }
    | 
    | define bar1 routine
    |   inputs x
    |   trashes x, z, n
    | {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar0
    |     } else {
    |         ld x, 2
    |         goto bar1
    |     }
    | }
    ? InconsistentExitError

Here is like the above, but the two routines have different inputs, and that's OK.

    | define bar0 routine
    |   inputs x
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld a, x
    | }
    | 
    | define bar1 routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld a, 200
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes x, z, n
    | {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar0
    |     } else {
    |         ld x, 2
    |         goto bar1
    |     }
    | }
    = ok

TODO: we should have a lot more test cases for the above, here.

Can't `goto` a routine that outputs or trashes more than the current routine.

    | define bar routine trashes x, y, z, n {
    |     ld x, 200
    |     ld y, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     goto bar
    | }
    ? IncompatibleConstraintsError

    | define bar routine outputs y trashes z, n {
    |     ld y, 200
    | }
    | 
    | define main routine trashes x, z, n {
    |     ld x, 0
    |     goto bar
    | }
    ? IncompatibleConstraintsError

Can `goto` a routine that outputs or trashes less than the current routine.

    | define bar routine trashes x, z, n {
    |     ld x, 1
    | }
    | 
    | define main routine trashes a, x, z, n {
    |     ld a, 0
    |     ld x, 0
    |     goto bar
    | }
    = ok

Indirect goto.

    | vector routine outputs x trashes a, z, n foo
    | 
    | define bar routine outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine outputs x trashes foo, a, z, n {
    |     copy bar, foo
    |     goto foo
    | }
    = ok

Jumping through the vector does indeed trash, or output, the things the
vector says it does.

    | vector routine
    |   trashes a, x, z, n
    |     foo
    | 
    | define bar routine
    |   trashes a, x, z, n {
    |     ld x, 200
    | }
    | 
    | define sub routine
    |   trashes foo, a, x, z, n {
    |     ld x, 0
    |     copy bar, foo
    |     goto foo
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes foo, x, z, n {
    |     call sub
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x

    | vector routine
    |   outputs x
    |   trashes a, z, n  foo
    | 
    | define bar routine
    |   outputs x
    |   trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define sub routine
    |   outputs x
    |   trashes foo, a, z, n {
    |     ld x, 0
    |     copy bar, foo
    |     goto foo
    | }
    | 
    | define main routine
    |   outputs a
    |   trashes foo, x, z, n {
    |     call sub
    |     ld a, x
    | }
    = ok

### vector tables ###

A vector can be copied into a vector table.

    | vector routine
    |   outputs x
    |   trashes a, z, n
    |     one
    | vector (routine
    |   outputs x
    |   trashes a, z, n)
    |     table[256] many
    | 
    | define bar routine outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, one
    |     copy one, many + x
    | }
    = ok

A vector can be copied out of a vector table.

    | vector routine
    |   outputs x
    |   trashes a, z, n
    |     one
    | vector (routine
    |   outputs x
    |   trashes a, z, n)
    |     table[256] many
    | 
    | define bar routine outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy many + x, one
    |     call one
    | }
    = ok

A routine can be copied into a vector table.

    | vector (routine
    |     outputs x
    |     trashes a, z, n)
    |   table[256] many
    | 
    | define bar routine outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, many + x
    | }
    = ok

A vector in a vector table cannot be directly called.

    | vector (routine
    |     outputs x
    |     trashes a, z, n)
    |   table[256] many
    | 
    | routine bar outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, many + x
    |     call many + x
    | }
    ? SyntaxError

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

### static ###

When memory locations are defined static to a routine, they cannot be
directly input, nor directly output; and since they are always initialized,
they cannot be trashed.  Thus, they really don't participate in the analysis.

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
