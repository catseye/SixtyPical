SixtyPical Analysis
===================

This is a test suite, written in [Falderal][] format, for the SixtyPical
static analysis rules.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Analyze SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --analyze --traceback %(test-body-file) && echo ok"

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
    ? UsageClashError: a

If a routine declares it outputs a location, that location should be initialized.

    | routine main
    |   outputs a, x, z, n
    | {
    |     ld x, 0
    | }
    ? UninitializedOutputError: a

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
    ? IllegalWriteError: x

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
    ? UninitializedAccessError: x

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
    ? IllegalWriteError: a

    | routine main
    |   trashes a, n
    | {
    |     ld a, 0
    | }
    ? IllegalWriteError: z

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
    ? UninitializedAccessError: x

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
    ? IllegalWriteError: lives

Storing to a table, you must use an index, and vice-versa.

    | byte one
    | byte table many
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
    | byte table many
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
    | byte table many
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
    | byte table many
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

Reading from a table, you must use an index, and vice-versa.

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

    | byte table many
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
    ? UninitializedAccessError: lives

    | byte lives
    | routine main
    |   inputs lives
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     add a, lives
    | }
    ? UninitializedAccessError: a

Can't `add` to a memory location that isn't writeable.

    | routine main
    |   inputs a
    |   trashes c
    | {
    |     st off, c
    |     add a, 0
    | }
    ? IllegalWriteError: a

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
    ? UninitializedAccessError: lives

    | byte lives
    | routine main
    |   inputs lives
    |   outputs a
    |   trashes c, z, v, n
    | {
    |     st off, c
    |     sub a, lives
    | }
    ? UninitializedAccessError: a

Can't `sub` to a memory location that isn't writeable.

    | routine main
    |   inputs a
    |   trashes c
    | {
    |     st off, c
    |     sub a, 0
    | }
    ? IllegalWriteError: a

### inc ###

Location must be initialized and writeable.

    | routine main
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    ? UninitializedAccessError: x

    | routine main
    |   inputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    ? IllegalWriteError: x

    | routine main
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    = ok

### dec ###

Location must be initialized and writeable.

    | routine main
    |   outputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    ? UninitializedAccessError: x

    | routine main
    |   inputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    ? IllegalWriteError: x

    | routine main
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     dec x
    | }
    = ok

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
    ? IllegalWriteError: c

    | routine main
    |   trashes z, c, n
    | {
    |     cmp a, 4
    | }
    ? UninitializedAccessError: a

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
    ? IllegalWriteError: a

    | routine main
    |   trashes z, n
    | {
    |     and a, 4
    | }
    ? UninitializedAccessError: a

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
    ? IllegalWriteError: a

    | routine main
    |   trashes z, n
    | {
    |     or a, 4
    | }
    ? UninitializedAccessError: a

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
    ? IllegalWriteError: a

    | routine main
    |   trashes z, n
    | {
    |     xor a, 4
    | }
    ? UninitializedAccessError: a

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
    ? IllegalWriteError: a

    | routine main
    |   inputs a
    |   outputs a, c, z, n
    | {
    |     shl a
    | }
    ? UninitializedAccessError: c

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
    ? IllegalWriteError: a

    | routine main
    |   inputs a
    |   outputs a, c, z, n
    | {
    |     shr a
    | }
    ? UninitializedAccessError: c

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
    ? UninitializedAccessError: x

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
    ? IllegalWriteError: lives

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
    ? UninitializedOutputError: lives

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
    ? UninitializedAccessError: x

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
    ? UninitializedAccessError: x

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
    ? UninitializedAccessError: a

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
    ? UninitializedAccessError: a

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
    ? UninitializedAccessError: y

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
    ? UninitializedAccessError: x

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
    ? IllegalWriteError: lives

a, z, and n are trashed, and must be declared as such

    | byte lives
    | routine main
    |   outputs lives
    | {
    |     copy 0, lives
    | }
    ? IllegalWriteError: a

a, z, and n are trashed, and must not be declared as outputs.

    | byte lives
    | routine main
    |   outputs lives, a, z, n
    | {
    |     copy 0, lives
    | }
    ? UninitializedOutputError: a

Unless of course you subsequently initialize them.

    | byte lives
    | routine main
    |   outputs lives, a, z, n
    | {
    |     copy 0, lives
    |     ld a, 0
    | }
    = ok

You can copy the address of a routine into a vector, if that vector is declared appropriately.

    | vector vec
    |   inputs x
    |   outputs x
    |   trashes z, n
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
    = ok

But not if the vector is declared inappropriately.

    | vector vec
    |   inputs y
    |   outputs y
    |   trashes z, n
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
    ? IncompatibleConstraintsError

Routines are read-only.

    | vector vec
    |   inputs x
    |   outputs x
    |   trashes z, n
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
    |     copy vec, foo
    | }
    ? TypeMismatchError

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
    = ok

Calling the vector has indeed trashed stuff etc,

    | vector foo trashes x, z, n
    | 
    | routine bar trashes x, z, n {
    |     ld x, 200
    | }
    | 
    | routine main inputs bar outputs x, foo trashes z, n {
    |     ld x, 0
    |     copy bar, foo
    |     call foo
    | }
    ? UninitializedOutputError: x

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
