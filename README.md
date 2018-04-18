SixtyPical
==========

_Version 0.16.  Work-in-progress, everything is subject to change._

**SixtyPical** is a 6502-like programming language with advanced
static analysis.

"6502-like" means that it has similar restrictions as programming
in 6502 assembly (e.g. the programmer must choose the registers that
values will be stored in) and is concomitantly easy for a compiler to
translate it to 6502 machine language code.

"Advanced static analysis" includes _abstract interpretation_, where we
go through the program step by step, tracking not just the changes that
happen during a _specific_ execution of the program, but _sets_ of changes
that could _possibly_ happen in any run of the program.  This lets us
determine that certain things can never happen, which we can then formulate
as safety checks.

In practice, this means it catches things like

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you call trashes a register you thought was preserved
*   you tried to read or write a byte beyond the end of a byte array
*   you tried to write the address of something that was not a routine, to
    a jump vector

and suchlike.  It also provides some convenient operations based on
machine-language programming idioms, such as

*   copying values from one register to another (via a third register when
    there are no underlying instructions that directly support it); this
    includes 16-bit values, which are copied in two steps
*   explicit tail calls
*   indirect subroutine calls

The reference implementation can analyze and compile SixtyPical programs to
6502 machine code.

Quick Start
-----------

If you have the [VICE][] emulator installed, from this directory, you can run

    ./loadngo.sh c64 eg/c64/hearts.60p

and it will compile the [hearts.60p source code](eg/c64/hearts.60p) and
automatically start it in the `x64` emulator, and you should see:

![Screenshot of result of running hearts.60p](https://raw.github.com/catseye/SixtyPical/master/images/hearts.png)

You can try the `loadngo.sh` script on other sources in the `eg` directory
tree, which contains more extensive examples, including an entire
game(-like program); see [eg/README.md](eg/README.md) for a listing.

[VICE]: http://vice-emu.sourceforge.net/

Documentation
-------------

*   [Design Goals](doc/Design%20Goals.md)
*   [SixtyPical specification](doc/SixtyPical.md)
*   [SixtyPical revision history](HISTORY.md)
*   [Literate test suite for SixtyPical syntax](tests/SixtyPical%20Syntax.md)
*   [Literate test suite for SixtyPical analysis](tests/SixtyPical%20Analysis.md)
*   [Literate test suite for SixtyPical compilation](tests/SixtyPical%20Compilation.md)
*   [Literate test suite for SixtyPical fallthru optimization](tests/SixtyPical%20Fallthru.md)
*   [6502 Opcodes used/not used in SixtyPical](doc/6502%20Opcodes.md)

TODO
----

### `low` and `high` address operators

To turn `word` type into `byte`.

Trying to remember if we have a compelling case for this or now.  The best I can think
of is for implementing 16-bit `cmp` in an efficient way.  Maybe we should see if we
can get by with 16-bit `cmp` instead though.

The problem is that once a byte is extracted, putting it back into a word is awkward.
The address operators have to modify a destination in a special way.  That is, when
you say `st a, >word`, you are updating `word` to be `word & $ff | a << 8`, somelike.
Is that consistent with `st`?  Well, probably it is, but we have to explain it.

### Save registers on stack

This preserves them, so that, semantically, they can be used later even though they
are trashed inside the block.

### Associate each pointer with the buffer it points into

Check that the buffer being read or written to through pointer, appears in appropriate inputs or outputs set.

### `static` tables

They are uninitialized, but the twist is, the address is a buffer that is
an input to and/or output of the routine.  So, they are defined (insofar
as the buffer is defined.)

They are therefore a "view" of a section of a buffer.

This is slightly dangerous since it does permit aliases: the buffer and the
table refer to the same memory.

`static` tables overlayed on buffers is an alternative to `static` pointers
(which are currently not possible because pointers must be zero-page, thus `@`, thus uninitialized.)

### Question "consistent initialization"

Question the value of the "consistent initialization" principle for `if` statement analysis.

### Tail-call optimization

More generally, define a block as having zero or one `goto`s at the end.  (and `goto`s cannot
appear elsewhere.)

If a block ends in a `call` can that be converted to end in a `goto`?  Why not?  I think it can.
The constraints should iron out the same both ways.

And - once we have this - why do we need `goto` to be in tail position, strictly?
As long as the routine has consistent type context every place it exits, that should be fine.

### Include pragmas

Search a searchlist of include paths.  And use them to make libraries of routines.

One such library routine might be an `interrupt routine` type for various architectures.
Since "the supervisor" has stored values on the stack, we should be able to trash them
with impunity, in such a routine.
