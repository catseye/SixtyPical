SixtyPical Analysis - Storage
=============================

This is a test suite, written in [Falderal][] format, for the SixtyPical
static analysis rules, with regard to storage (load, store, tables, etc.)

[Falderal]:     http://catseye.tc/node/Falderal

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

Storing to a table, you may also include a constant offset.

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many + 100 + x
    | }
    = ok

Reading from a table, you may also include a constant offset.

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, many + 100 + x
    | }
    = ok

Using a constant offset, you can read and write entries in
the table beyond the 256th.

    | byte one
    | byte table[1024] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, many + 999 + x
    |     st a, many + 1000 + x
    | }
    = ok

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

Copying to and from a word table with a constant offset.

    | word one
    | word table[256] many
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy one, many + 100 + x
    |     copy many + 100 + x, one
    |     copy 9999, many + 1 + x
    | }
    = ok

#### tables: range checking ####

It is a static analysis error if it cannot be proven that a read or write
to a table falls within the defined size of that table.

If a table has 256 entries, then there is never a problem (so long as
no constant offset is supplied), because a byte cannot index any entry
outside of 0..255.

But if the table has fewer than 256 entries, or if a constant offset is
supplied, there is the possibility that the index will refer to an
entry in the table which does not exist.

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

Any constant offset is taken into account in this check.

    | byte table[32] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 31
    |     ld a, many + 1 + x
    | }
    ? RangeExceededError

    | byte table[32] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 31
    |     ld a, 0
    |     st a, many + 1 + x
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

Any constant offset is taken into account in this check.

    | word one: 77
    | word table[32] many
    | 
    | define main routine
    |   inputs many, one
    |   outputs many, one
    |   trashes a, x, n, z
    | {
    |     ld x, 31
    |     copy many + 1 + x, one
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
    |     ld x, 31
    |     copy one, many + 1 + x
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

Tests for "clipping", but not enough.

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
    |     copy one, many + 1 + x
    |     copy many + 1 + x, one
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

### point ... into blocks ###

Pointer must be a pointer type.

    | byte table[256] tab
    | word ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs y, tab
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
    | }
    ? TypeMismatchError

Cannot write through pointer outside a `point ... into` block.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab, ptr
    |   outputs y, tab
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy 123, [ptr] + y
    | }
    ? ForbiddenWriteError

After a `point ... into` block, the pointer is no longer meaningful.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs y, tab
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
    |     copy 123, [ptr] + y
    | }
    ? UnmeaningfulReadError: ptr

Write literal through a pointer into a table.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs y, tab
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
    | }
    = ok

Writing into a table via a pointer does use `y`.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, z, n, ptr
    | {
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
    | }
    ? UnmeaningfulReadError

Write stored value through a pointer into a table.

    | byte table[256] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs foo, tab
    |   outputs y, tab
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy foo, [ptr] + y
    |     }
    | }
    = ok

Read a table entry via a pointer.

    | byte table[256] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs foo
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy [ptr] + y, foo
    |     }
    | }
    = ok

Read and write through two pointers into a table.

    | byte table[256] tab
    | pointer ptra
    | pointer ptrb
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, z, n, ptra, ptrb
    | {
    |     ld y, 0
    |     point ptra into tab {
    |         reset ptra 0
    |         point ptrb into tab {
    |             reset ptrb 0
    |             copy [ptra] + y, [ptrb] + y
    |         }
    |     }
    | }
    = ok

Read through a pointer into a table, to the `a` register.  Note that this is done with `ld`,
not `copy`.

    | byte table[256] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs a
    |   trashes y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         ld a, [ptr] + y
    |     }
    | }
    = ok

Write the `a` register through a pointer into a table.  Note that this is done with `st`,
not `copy`.

    | byte table[256] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         ld a, 255
    |         st a, [ptr] + y
    |     }
    | }
    = ok

Cannot get a pointer into a non-byte (for instance, word) table.

    | word table[256] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs foo
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy [ptr] + y, foo
    |     }
    | }
    ? TypeMismatchError

Cannot get a pointer into a non-byte (for instance, vector) table.

    | vector (routine trashes a, z, n) table[256] tab
    | pointer ptr
    | vector (routine trashes a, z, n) foo
    | 
    | define main routine
    |   inputs tab
    |   outputs foo
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy [ptr] + y, foo
    |     }
    | }
    ? TypeMismatchError

`point into` by itself only requires `ptr` to be writeable.  By itself,
it does not require `tab` to be readable or writeable.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   trashes a, z, n, ptr
    | {
    |     point ptr into tab {
    |         ld a, 0
    |     }
    | }
    = ok

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     point ptr into tab {
    |         ld a, 0
    |     }
    | }
    = ok

It does need to be accessible by the routine if you're going to reset the pointer
to it though.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   trashes a, z, n
    | {
    |     point ptr into tab {
    |         reset ptr 0
    |         ld a, 0
    |     }
    | }
    ? UnmeaningfulReadError: tab

And the pointer needs to be writeable if it's going to be reset as well.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   trashes a, z, n
    | {
    |     point ptr into tab {
    |         reset ptr 0
    |         ld a, 0
    |     }
    | }
    ? ForbiddenWriteError

After a `point into` block, the pointer is no longer meaningful and cannot
be considered an output of the routine.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs y, tab, ptr
    |   trashes a, z, n
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
    | }
    ? UnmeaningfulOutputError

If code in a routine reads from a table through a pointer, the table must be in
the `inputs` of that routine.

    | byte table[256] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   outputs foo
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy [ptr] + y, foo
    |     }
    | }
    ? UnmeaningfulReadError

Likewise, if code in a routine writes into a table via a pointer, the table must
be in the `outputs` of that routine.

    | byte table[256] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
    | }
    ? ForbiddenWriteError

If code in a routine reads from a table through a pointer, the pointer *should*
remain inside the range of the  table.  This is currently not checked.

    | byte table[32] tab
    | pointer ptr
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs foo
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         st off, c
    |         add ptr, word 100
    |         copy [ptr] + y, foo
    |     }
    | }
    = ok

Likewise, if code in a routine writes into a table through a pointer, the pointer
*should* remain inside the range of the  table.  This is currently not checked.

    | byte table[32] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         st off, c
    |         add ptr, word 100
    |         copy 123, [ptr] + y
    |     }
    | }
    = ok

### reset ###

Can't have a `reset` outside a `point ... into` block.

    | byte table[32] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     reset ptr 0
    |     point ptr into tab {
    |         st off, c
    |         add ptr, word 10
    |         copy 123, [ptr] + y
    |     }
    | }
    ? ForbiddenWriteError: ptr

Can't write into a table if the pointer hasn't been `reset`; the
pointer is not meaningful until it's `reset`.

    | byte table[32] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         st off, c
    |         copy 123, [ptr] + y
    |     }
    | }
    ? UnmeaningfulReadError: ptr

Can't read from a table if the pointer hasn't been reset.

    | byte table[32] tab
    | pointer ptr
    | byte ou
    | 
    | define main routine
    |   inputs tab
    |   outputs tab, ou
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         st off, c
    |         copy [ptr] + y, ou
    |     }
    | }
    ? UnmeaningfulReadError: ptr

Multiple `reset`s may occur inside the same block.

    | byte table[32] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 10
    |         copy 123, [ptr] + y
    |         reset ptr 20
    |         copy 35, [ptr] + y
    |     }
    | }
    = ok

The offset in `reset` may not exceed the table's size.

    | byte table[32] tab
    | pointer ptr
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, c, z, n, v, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 32
    |         copy 123, [ptr] + y
    |     }
    | }
    ? RangeExceededError

### locals ###

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

When memory locations are defined local to a routine, but not static,
they cannot be directly input, nor directly output; but they are considered
uninitialized from the time the routine is called to until a value is stored
in them.

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
    = ok

    | define main routine
    |   outputs x
    |   trashes z, n
    |   local byte t
    | {
    |   inc t
    |   ld x, t
    | }
    ? UnmeaningfulReadError: t

Local non-statics can be meaningully given an explicit address.

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

    | define main routine
    |   outputs x
    |   trashes z, n
    |   local byte t @ 1024
    | {
    |   inc t
    |   ld x, t
    | }
    ? UnmeaningfulReadError: t

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
