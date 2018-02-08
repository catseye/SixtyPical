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

    | routine up
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, 1
    | }
    = ok

Routines may not declare a memory location to be both an output and trashed.

    | routine main
    |   outputs a
    |   trashes a
    | {
    |     ld a, 0
    | }
    ? InconsistentConstraintsError: a

If a routine declares it outputs a location, that location should be initialized.

    | routine main
    |   outputs a, x, z, n
    | {
    |     ld x, 0
    | }
    ? UnmeaningfulOutputError: a in main

    | routine main
    |   inputs a
    |   outputs a
    | {
    | }
    = ok

If a routine declares it outputs a location, that location may or may not have
been initialized.  Trashing is mainly a signal to the caller.

    | routine main
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | routine main
    |   trashes x, z, n
    | {
    | }
    = ok

If a routine modifies a location, it needs to either output it or trash it.

    | routine main
    | {
    |     ld x, 0
    | }
    ? ForbiddenWriteError: x in main

    | routine main
    |   outputs x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | routine main
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

This is true regardless of whether it's an input or not.

    | routine main
    |   inputs x
    | {
    |     ld x, 0
    | }
    ? ForbiddenWriteError: x in main

    | routine main
    |   inputs x
    |   outputs x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | routine main
    |   inputs x
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

If a routine trashes a location, this must be declared.

    | routine foo
    |   trashes x
    | {
    |     trash x
    | }
    = ok

    | routine foo
    | {
    |     trash x
    | }
    ? ForbiddenWriteError: x in foo

    | routine foo
    |   outputs x
    | {
    |     trash x
    | }
    ? UnmeaningfulOutputError: x in foo

If a routine causes a location to be trashed, this must be declared in the caller.

    | routine trash_x
    |   trashes x, z, n
    | {
    |   ld x, 0
    | }
    | 
    | routine foo
    |   trashes x, z, n
    | {
    |     call trash_x
    | }
    = ok

    | routine trash_x
    |   trashes x, z, n
    | {
    |   ld x, 0
    | }
    | 
    | routine foo
    |   trashes z, n
    | {
    |     call trash_x
    | }
    ? ForbiddenWriteError: x in foo

    | routine trash_x
    |   trashes x, z, n
    | {
    |   ld x, 0
    | }
    | 
    | routine foo
    |   outputs x
    |   trashes z, n
    | {
    |     call trash_x
    | }
    ? UnmeaningfulOutputError: x in foo

If a routine reads or writes a user-define memory location, it needs to declare that too.

    | byte b1 @ 60000
    | byte b2 : 3
    | word w1 @ 60001
    | word w2 : 2000
    | 
    | routine main
    |   inputs b1, w1
    |   outputs b2, w2
    |   trashes a, z, n
    | {
    |   ld a, b1
    |   st a, b2
    |   copy w1, w2
    | }
    = ok

### ld ###

Can't `ld` from a memory location that isn't initialized.

    | routine main
    |   inputs a, x
    |   trashes a, z, n
    | {
    |     ld a, x
    | }
    = ok

    | routine main
    |   inputs a
    |   trashes a
    | {
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x in main

Can't `ld` to a memory location that doesn't appear in (outputs ∪ trashes).

    | routine main
    |   trashes a, z, n
    | {
    |     ld a, 0
    | }
    = ok

    | routine main
    |   outputs a
    |   trashes z, n
    | {
    |     ld a, 0
    | }
    = ok

    | routine main
    |   outputs z, n
    |   trashes a
    | {
    |     ld a, 0
    | }
    = ok

    | routine main
    |   trashes z, n
    | {
    |     ld a, 0
    | }
    ? ForbiddenWriteError: a in main

    | routine main
    |   trashes a, n
    | {
    |     ld a, 0
    | }
    ? ForbiddenWriteError: z in main

Can't `ld` a `word` type.

    | word foo
    | 
    | routine main
    |   inputs foo
    |   trashes a, n, z
    | {
    |     ld a, foo
    | }
    ? TypeMismatchError: foo and a in main

### st ###

Can't `st` from a memory location that isn't initialized.

    | byte lives
    | routine main
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    = ok

    | byte lives
    | routine main
    |   trashes x, lives
    | {
    |     st x, lives
    | }
    ? UnmeaningfulReadError: x in main

Can't `st` to a memory location that doesn't appear in (outputs ∪ trashes).

    | byte lives
    | routine main
    |   trashes lives
    | {
    |     st 0, lives
    | }
    = ok

    | byte lives
    | routine main
    |   outputs lives
    | {
    |     st 0, lives
    | }
    = ok

    | byte lives
    | routine main
    |   inputs lives
    | {
    |     st 0, lives
    | }
    ? ForbiddenWriteError: lives in main

Can't `st` a `word` type.

    | word foo
    | 
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld a, many + x
    | }
    ? UnmeaningfulReadError: x

Copying to and from a word table.

    | word one
    | word table[256] many
    | 
    | routine main
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
    | routine main
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
    | routine main
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy one + x, many
    | }
    ? TypeMismatchError

You can also copy a literal word to a word table.

    | word table[256] many
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

### add ###

Can't `add` from or to a memory location that isn't initialized.

    | routine main
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, 0
    | }
    = ok

    | byte lives
    | routine main
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, lives
    | }
    ? UnmeaningfulReadError: lives in main

    | byte lives
    | routine main
    |   inputs lives
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, lives
    | }
    ? UnmeaningfulReadError: a in main

Can't `add` to a memory location that isn't writeable.

    | routine main
    |   inputs a
    |   trashes c
    | {
    |     st off, c
    |     add a, 0
    | }
    ? ForbiddenWriteError: a in main

You can `add` a word constant to a word memory location.

    | word score
    | routine main
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
    | routine main
    |   inputs a, score
    |   outputs score, a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    ? UnmeaningfulOutputError: a in main

To be sure, `add`ing a word constant to a word memory location trashes `a`.

    | word score
    | routine main
    |   inputs score
    |   outputs score
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    ? ForbiddenWriteError: a in main

You can `add` a word memory location to another word memory location.

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
    = ok

`add`ing a word memory location to a word memory location trashes `a`.

    | word score
    | word delta
    | routine main
    |   inputs score, delta
    |   outputs score
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add score, delta
    | }
    ? ForbiddenWriteError: a in main

You can `add` a word memory location, or a constant, to a pointer.

    | pointer ptr
    | word delta
    | routine main
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
    | routine main
    |   inputs ptr, delta
    |   outputs ptr
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add ptr, delta
    |     add ptr, word 1
    | }
    ? ForbiddenWriteError: a in main

### sub ###

Can't `sub` from or to a memory location that isn't initialized.

    | routine main
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, 0
    | }
    = ok

    | byte lives
    | routine main
    |   inputs a
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, lives
    | }
    ? UnmeaningfulReadError: lives in main

    | byte lives
    | routine main
    |   inputs lives
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, lives
    | }
    ? UnmeaningfulReadError: a in main

Can't `sub` to a memory location that isn't writeable.

    | routine main
    |   inputs a
    |   trashes c
    | {
    |     st off, c
    |     sub a, 0
    | }
    ? ForbiddenWriteError: a in main

You can `sub` a word constant from a word memory location.

    | word score
    | routine main
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
    | routine main
    |   inputs a, score
    |   outputs score, a
    |   trashes c, z, v, n
    | {
    |     st on, c
    |     sub score, 1999
    | }
    ? UnmeaningfulOutputError: a in main

You can `sub` a word memory location from another word memory location.

    | word score
    | word delta
    | routine main
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
    | routine main
    |   inputs score, delta
    |   outputs score
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub score, delta
    | }
    ? ForbiddenWriteError: a in main

### inc ###

Location must be initialized and writeable.

    | routine main
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    ? UnmeaningfulReadError: x in main

    | routine main
    |   inputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    ? ForbiddenWriteError: x in main

    | routine main
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
    | routine main
    |   inputs foo
    |   outputs foo
    |   trashes z, n
    | {
    |     inc foo
    | }
    ? TypeMismatchError: foo in main

### dec ###

Location must be initialized and writeable.

    | routine main
    |   outputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    ? UnmeaningfulReadError: x in main

    | routine main
    |   inputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    ? ForbiddenWriteError: x in main

    | routine main
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
    | routine main
    |   inputs foo
    |   outputs foo
    |   trashes z, n
    | {
    |     dec foo
    | }
    ? TypeMismatchError: foo in main

### cmp ###

Some rudimentary tests for cmp.

    | routine main
    |   inputs a
    |   trashes z, c, n
    | {
    |     cmp a, 4
    | }
    = ok

    | routine main
    |   inputs a
    |   trashes z, n
    | {
    |     cmp a, 4
    | }
    ? ForbiddenWriteError: c in main

    | routine main
    |   trashes z, c, n
    | {
    |     cmp a, 4
    | }
    ? UnmeaningfulReadError: a in main

### and ###

Some rudimentary tests for and.

    | routine main
    |   inputs a
    |   outputs a, z, n
    | {
    |     and a, 4
    | }
    = ok

    | routine main
    |   inputs a
    |   trashes z, n
    | {
    |     and a, 4
    | }
    ? ForbiddenWriteError: a in main

    | routine main
    |   trashes z, n
    | {
    |     and a, 4
    | }
    ? UnmeaningfulReadError: a in main

### or ###

Writing unit tests on a train.  Wow.

    | routine main
    |   inputs a
    |   outputs a, z, n
    | {
    |     or a, 4
    | }
    = ok

    | routine main
    |   inputs a
    |   trashes z, n
    | {
    |     or a, 4
    | }
    ? ForbiddenWriteError: a in main

    | routine main
    |   trashes z, n
    | {
    |     or a, 4
    | }
    ? UnmeaningfulReadError: a in main

### xor ###

Writing unit tests on a train.  Wow.

    | routine main
    |   inputs a
    |   outputs a, z, n
    | {
    |     xor a, 4
    | }
    = ok

    | routine main
    |   inputs a
    |   trashes z, n
    | {
    |     xor a, 4
    | }
    ? ForbiddenWriteError: a in main

    | routine main
    |   trashes z, n
    | {
    |     xor a, 4
    | }
    ? UnmeaningfulReadError: a in main

### shl ###

Some rudimentary tests for shl.

    | routine main
    |   inputs a, c
    |   outputs a, c, z, n
    | {
    |     shl a
    | }
    = ok

    | routine main
    |   inputs a, c
    |   outputs c, z, n
    | {
    |     shl a
    | }
    ? ForbiddenWriteError: a in main

    | routine main
    |   inputs a
    |   outputs a, c, z, n
    | {
    |     shl a
    | }
    ? UnmeaningfulReadError: c in main

### shr ###

Some rudimentary tests for shr.

    | routine main
    |   inputs a, c
    |   outputs a, c, z, n
    | {
    |     shr a
    | }
    = ok

    | routine main
    |   inputs a, c
    |   outputs c, z, n
    | {
    |     shr a
    | }
    ? ForbiddenWriteError: a in main

    | routine main
    |   inputs a
    |   outputs a, c, z, n
    | {
    |     shr a
    | }
    ? UnmeaningfulReadError: c in main

### call ###

When calling a routine, all of the locations it lists as inputs must be
initialized.

    | byte lives
    | 
    | routine foo
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | routine main
    | {
    |     call foo
    | }
    ? UnmeaningfulReadError: x in main

Note that if you call a routine that trashes a location, you also trash it.

    | byte lives
    | 
    | routine foo
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | routine main
    |   outputs x, z, n
    | {
    |     ld x, 0
    |     call foo
    | }
    ? ForbiddenWriteError: lives in main

    | byte lives
    | 
    | routine foo
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | routine main
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
    | routine foo
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | routine main
    |   outputs x, z, n, lives
    | {
    |     ld x, 0
    |     call foo
    | }
    ? UnmeaningfulOutputError: lives in main

...unless you write to it yourself afterwards.

    | byte lives
    | 
    | routine foo
    |   inputs x
    |   trashes lives
    | {
    |     st x, lives
    | }
    | 
    | routine main
    |   outputs x, z, n, lives
    | {
    |     ld x, 0
    |     call foo
    |     st x, lives
    | }
    = ok

If a routine declares outputs, they are initialized in the caller after
calling it.

    | routine foo
    |   outputs x, z, n
    | {
    |     ld x, 0
    | }
    | 
    | routine main
    |   outputs a
    |   trashes x, z, n
    | {
    |     call foo
    |     ld a, x
    | }
    = ok

    | routine foo
    | {
    | }
    | 
    | routine main
    |   outputs a
    |   trashes x
    | {
    |     call foo
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x in main

If a routine trashes locations, they are uninitialized in the caller after
calling it.

    | routine foo
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    = ok

    | routine foo
    |   trashes x, z, n
    | {
    |     ld x, 0
    | }
    | 
    | routine main
    |   outputs a
    |   trashes x, z, n
    | {
    |     call foo
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x in main

Calling an extern is just the same as calling a defined routine with the
same constraints.

    | routine chrout
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | routine main
    |   trashes a, z, n
    | {
    |     ld a, 65
    |     call chrout
    | }
    = ok

    | routine chrout
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | routine main
    |   trashes a, z, n
    | {
    |     call chrout
    | }
    ? UnmeaningfulReadError: a in main

    | routine chrout
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | routine main
    |   trashes a, x, z, n
    | {
    |     ld a, 65
    |     call chrout
    |     ld x, a
    | }
    ? UnmeaningfulReadError: a in main

### trash ###

Trash does nothing except indicate that we do not care about the value anymore.

    | routine foo
    |   inputs a
    |   outputs x
    |   trashes a, z, n
    | {
    |     st a, x
    |     ld a, 0
    |     trash a
    | }
    = ok

    | routine foo
    |   inputs a
    |   outputs a, x
    |   trashes z, n
    | {
    |     st a, x
    |     ld a, 0
    |     trash a
    | }
    ? UnmeaningfulOutputError: a in foo

    | routine foo
    |   inputs a
    |   outputs x
    |   trashes a, z, n
    | {
    |     st a, x
    |     trash a
    |     st a, x
    | }
    ? UnmeaningfulReadError: a in foo

### if ###

Both blocks of an `if` are analyzed.

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
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

    | routine foo
    |   inputs a, x, z
    |   trashes a
    | {
    |     if z {
    |         trash a
    |     } else {
    |         trash x
    |     }
    | }
    ? ForbiddenWriteError: x in foo

    | routine foo
    |   inputs a, x, z
    |   trashes x
    | {
    |     if z {
    |         trash a
    |     } else {
    |         trash x
    |     }
    | }
    ? ForbiddenWriteError: a in foo

### repeat ###

Repeat loop.

    | routine main
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

    | routine main
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

    | routine foo
    |   trashes y
    | {
    | }
    | 
    | routine main
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
    ? UnmeaningfulReadError: y in main

And if you trash the test expression (i.e. `z` in the below) inside the loop,
this is an error too.

    | word one : 0
    | word two : 0
    | 
    | routine main
    |   inputs one, two
    |   outputs two
    |   trashes a, z, n
    | {
    |     repeat {
    |         copy one, two
    |     } until z
    | }
    ? UnmeaningfulReadError: z in main

The body of `repeat forever` can be empty.

    | routine main
    | {
    |     repeat {
    |     } forever
    | }
    = ok

### copy ###

Can't `copy` from a memory location that isn't initialized.

    | byte lives
    | routine main
    |   inputs x
    |   outputs lives
    |   trashes a, z, n
    | {
    |     copy x, lives
    | }
    = ok

    | byte lives
    | routine main
    |   outputs lives
    |   trashes x, a, z, n
    | {
    |     copy x, lives
    | }
    ? UnmeaningfulReadError: x in main

Can't `copy` to a memory location that doesn't appear in (outputs ∪ trashes).

    | byte lives
    | routine main
    |   trashes lives, a, z, n
    | {
    |     copy 0, lives
    | }
    = ok

    | byte lives
    | routine main
    |   outputs lives
    |   trashes a, z, n
    | {
    |     copy 0, lives
    | }
    = ok

    | byte lives
    | routine main
    |   inputs lives
    |   trashes a, z, n
    | {
    |     copy 0, lives
    | }
    ? ForbiddenWriteError: lives in main

a, z, and n are trashed, and must be declared as such

    | byte lives
    | routine main
    |   outputs lives
    | {
    |     copy 0, lives
    | }
    ? ForbiddenWriteError: n in main

a, z, and n are trashed, and must not be declared as outputs.

    | byte lives
    | routine main
    |   outputs lives, a, z, n
    | {
    |     copy 0, lives
    | }
    ? UnmeaningfulOutputError: n in main

Unless of course you subsequently initialize them.

    | byte lives
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
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
    | routine main
    |   inputs buf
    |   outputs foo
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy [ptr] + y, foo
    | }
    = ok

Read through a pointer to the `a` register.  Note that this is done with `ld`,
not `copy`.

    | buffer[2048] buf
    | pointer ptr
    | byte foo
    | 
    | routine main
    |   inputs buf
    |   outputs a
    |   trashes y, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     ld a, [ptr] + y
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
    | routine foo
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |   inc x
    | }
    | 
    | routine main
    |   inputs foo
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? ConstantConstraintError: foo in main

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
    | 
    | routine foo
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |   inc x
    | }
    | 
    | routine main
    |   outputs vec, foo
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    ? ConstantConstraintError: foo in main

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
    | 
    | routine foo
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |   inc x
    | }
    | 
    | routine main
    |   outputs vec
    |   trashes a, z, n, foo
    | {
    |     copy foo, vec
    | }
    ? ConstantConstraintError: foo in main

You can copy the address of a routine into a vector, if that vector is
declared appropriately.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
    | 
    | routine foo
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

But not if the vector is declared inappropriately.

    | vector routine
    |   inputs y
    |   outputs y
    |   trashes z, n
    |     vec
    | 
    | routine foo
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
    ? IncompatibleConstraintsError

"Appropriately" means, if the routine affects no more than what is named
in the input/output sets of the vector.

    | vector routine
    |   inputs a, x
    |   outputs x
    |   trashes a, z, n
    |     vec
    | 
    | routine foo
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

Routines are read-only.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     vec
    | 
    | routine foo
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
    |     copy vec, foo
    | }
    ? TypeMismatchError

Indirect call.

    | vector routine
    |   outputs x trashes z, n
    |     foo
    | 
    | routine bar outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | routine main outputs x, foo trashes a, z, n {
    |     copy bar, foo
    |     call foo
    | }
    = ok

Calling the vector does indeed trash the things the vector says it does.

    | vector routine trashes x, z, n foo
    | 
    | routine bar trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | routine main outputs x, foo trashes z, n {
    |     ld x, 0
    |     copy bar, foo
    |     call foo
    | }
    ? UnmeaningfulOutputError: x in main

`goto`, if present, must be in tail position (the final instruction in a routine.)

    | routine bar trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | routine main trashes x, z, n {
    |     ld x, 0
    |     goto bar
    | }
    = ok

    | routine bar trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | routine main trashes x, z, n {
    |     goto bar
    |     ld x, 0
    | }
    ? IllegalJumpError

    | routine bar trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | routine main trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     }
    | }
    = ok

    | routine bar trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | routine main trashes x, z, n {
    |     ld x, 0
    |     if z {
    |         ld x, 1
    |         goto bar
    |     }
    |     ld x, 0
    | }
    ? IllegalJumpError

Can't `goto` a routine that outputs or trashes more than the current routine.

    | routine bar trashes x, y, z, n {
    |     ld x, 200
    |     ld y, 200
    | }
    | 
    | routine main trashes x, z, n {
    |     ld x, 0
    |     goto bar
    | }
    ? IncompatibleConstraintsError

    | routine bar outputs y trashes z, n {
    |     ld y, 200
    | }
    | 
    | routine main trashes x, z, n {
    |     ld x, 0
    |     goto bar
    | }
    ? IncompatibleConstraintsError

Can `goto` a routine that outputs or trashes less than the current routine.

    | routine bar trashes x, z, n {
    |     ld x, 1
    | }
    | 
    | routine main trashes a, x, z, n {
    |     ld a, 0
    |     ld x, 0
    |     goto bar
    | }
    = ok

Indirect goto.

    | vector routine outputs x trashes a, z, n foo
    | 
    | routine bar outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | routine main outputs x trashes foo, a, z, n {
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
    | routine bar
    |   trashes a, x, z, n {
    |     ld x, 200
    | }
    | 
    | routine sub
    |   trashes foo, a, x, z, n {
    |     ld x, 0
    |     copy bar, foo
    |     goto foo
    | }
    | 
    | routine main
    |   outputs a
    |   trashes foo, x, z, n {
    |     call sub
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x in main

    | vector routine
    |   outputs x
    |   trashes a, z, n  foo
    | 
    | routine bar
    |   outputs x
    |   trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | routine sub
    |   outputs x
    |   trashes foo, a, z, n {
    |     ld x, 0
    |     copy bar, foo
    |     goto foo
    | }
    | 
    | routine main
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
    | routine bar outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | routine main
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
    | routine bar outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | routine main
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
    | routine bar outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | routine main
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
    | routine main
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, many + x
    |     call many + x
    | }
    ? ValueError

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
    | routine foo
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
    | routine main
    |   outputs vec
    |   trashes a, z, n
    | {
    |     copy foo, vec
    | }
    = ok
