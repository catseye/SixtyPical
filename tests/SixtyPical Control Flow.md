SixtyPical Analysis - Control Flow
==================================

<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

This is a test suite, written in [Falderal][] format, for the SixtyPical
static analysis rules, with regard to flow of control.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Analyze SixtyPical program"

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

If a location is initialized in one block, it must be initialized in the other as well
in order to be considered to be initialized after the block.  If it is not consistent,
it will be considered uninitialized.

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
    ? UnmeaningfulOutputError: x

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
    ? UnmeaningfulOutputError: x

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
    ? UnmeaningfulOutputError: x

    | define foo routine
    |   inputs a
    |   trashes a, x, z, n, c
    | {
    |     cmp a, 42
    |     if not z {
    |         ld a, 6
    |     } else {
    |         ld x, 7
    |     }
    |     ld a, x
    | }
    ? UnmeaningfulReadError: x

If we don't care if it's uninitialized after the `if`, that's okay then.

    | define foo routine
    |   inputs a
    |   trashes a, x, z, n, c
    | {
    |     cmp a, 42
    |     if not z {
    |         ld a, 6
    |     } else {
    |         ld x, 7
    |     }
    | }
    = ok

Or, if it does get initialized on both branches, that's okay then.

    | define foo routine
    |   inputs a
    |   outputs x
    |   trashes a, z, n, c
    | {
    |     cmp a, 42
    |     if not z {
    |         ld x, 0
    |         ld a, 6
    |     } else {
    |         ld x, 7
    |     }
    | }
    = ok

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
    ? UnmeaningfulOutputError: x

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

And in particular, you can't uninitialize the loop variable, in the loop.

    | define foo routine
    |   trashes x
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
    ? ForbiddenWriteError: x

So, if you call a routine from inside the loop, it better not also
loop on the same variable.

    | define foo routine
    |   inputs y
    |   outputs x, y, n, z
    |   trashes c
    | {
    |     ld x, 0
    |     for x up to 15 {
    |         inc y
    |     }
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
    ? ForbiddenWriteError: x

But if you take care to save and restore the loop variable in the
called routine, it will be okay.

    | define foo routine
    |   inputs y
    |   outputs y, n, z
    |   trashes a, c
    | {
    |     save x {
    |         ld x, 0
    |         for x up to 15 {
    |             inc y
    |         }
    |     }
    | }
    | 
    | define main routine
    |   outputs x, y, n, z
    |   trashes a, c
    | {
    |     ld x, 0
    |     ld y, 15
    |     for x up to 15 {
    |         inc y
    |         call foo
    |     }
    | }
    = ok

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

Another inconsistent exit test, this one based on "real" code
(the `ribos2` demo).

    | typedef routine
    |   inputs border_color, vic_intr
    |   outputs border_color, vic_intr
    |   trashes a, z, n, c
    |     irq_handler
    | 
    | vector irq_handler cinv @ $314
    | vector irq_handler saved_irq_vec
    | byte vic_intr         @ $d019
    | byte border_color     @ $d020
    | 
    | define pla_tay_pla_tax_pla_rti routine
    |   inputs a
    |   trashes a
    |     @ $EA81
    | 
    | define our_service_routine irq_handler
    | {
    |     ld a, vic_intr
    |     st a, vic_intr
    |     and a, 1
    |     cmp a, 1
    |     if not z {
    |         goto saved_irq_vec
    |     } else {
    |         ld a, border_color
    |         xor a, $ff
    |         st a, border_color
    |         goto pla_tay_pla_tax_pla_rti
    |     }
    | }
    | 
    | define main routine
    | {
    | }
    ? InconsistentExitError

    | typedef routine
    |   inputs border_color, vic_intr
    |   outputs border_color, vic_intr
    |   trashes a, z, n, c
    |     irq_handler
    | 
    | vector irq_handler cinv @ $314
    | vector irq_handler saved_irq_vec
    | byte vic_intr         @ $d019
    | byte border_color     @ $d020
    | 
    | define pla_tay_pla_tax_pla_rti routine
    |   inputs border_color, vic_intr
    |   outputs border_color, vic_intr
    |   trashes a, z, n, c
    |     @ $EA81
    | 
    | define our_service_routine irq_handler
    | {
    |     ld a, vic_intr
    |     st a, vic_intr
    |     and a, 1
    |     cmp a, 1
    |     if not z {
    |         goto saved_irq_vec
    |     } else {
    |         ld a, border_color
    |         xor a, $ff
    |         st a, border_color
    |         goto pla_tay_pla_tax_pla_rti
    |     }
    | }
    | 
    | define main routine
    | {
    | }
    = ok

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
