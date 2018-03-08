SixtyPical
==========

_Version 0.13.  Work-in-progress, everything is subject to change._

**SixtyPical** is a 6502-assembly-like programming language with advanced
static analysis.

"6502-assembly-like" means that it has similar restrictions as programming
in 6502 assembly (e.g. the programmer must choose the registers that
values will be stored in) and is concomitantly easy for a compiler to
translate it to 6502 machine language code.

"Advanced static analysis" includes _abstract interpretation_, where we
go through the program step by step, tracking not just the changes that
happen during a _specific_ execution of the program, but _sets_ of changes
that could _possibly_ happen in any run of the program.  This lets us
determine that certain things can never happen, which we can present as
safety guarantees.

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
tree.  There is an entire small game(-like program) in [demo-game.60p](eg/c64/demo-game.60p).

Documentation
-------------

*   [Design Goals](doc/Design%20Goals.md)
*   [SixtyPical specification](doc/SixtyPical.md)
*   [SixtyPical revision history](HISTORY.md)
*   [Literate test suite for SixtyPical syntax](tests/SixtyPical%20Syntax.md)
*   [Literate test suite for SixtyPical analysis](tests/SixtyPical%20Analysis.md)
*   [Literate test suite for SixtyPical compilation](tests/SixtyPical%20Compilation.md)
*   [6502 Opcodes used/not used in SixtyPical](doc/6502%20Opcodes.md)

TODO
----

### `for`-like loop

We have range-checking in the abstract analysis now, but we lack practical ways
to use it.

We can `and` a value to ensure it is within a certain range.  However, in the 6502
ISA the only register you can `and` is `A`, while loops are done with `X` or `Y`.
Insisting this as the way to do it would result in a lot of `TXA`s and `TAX`s.

What would be better is a dedicated `for` loop, like

    for x in 0 to 15 {
        // in here, we know the range of x is exactly 0-15 inclusive
        // also in here: we are disallowed from changing x
    }

However, this is slightly restrictive, and hides a lot.

However however, options which do not hide a lot, require a lot of looking at
(to ensure: did you increment the loop variable? only once? etc.)

The leading compromise so far is an "open-faced for loop", like

    ld x, 15
    for x downto 0 {
        // same as above
    }

This makes it a little more explicit, at least, even though the loop
decrementation is still hidden.

### Save registers on stack

This preserves them, so that, semantically, they can be used later even though they
are trashed inside the block.

### Re-order routines and optimize tail-calls to fallthroughs

Not because it saves 3 bytes, but because it's a neat trick.  Doing it optimally
is probably NP-complete.  But doing it adequately is probably not that hard.

### And at some point...

*   `low` and `high` address operators - to turn `word` type into `byte`.
*   `const`s that can be used in defining the size of tables, etc.
*   Tests, and implementation, ensuring a routine can be assigned to a vector of "wider" type
*   Related: can we simply view a (small) part of a buffer as a byte table?  If not, why not?
*   Check that the buffer being read or written to through pointer, appears in approporiate inputs or outputs set.
    (Associate each pointer with the buffer it points into.)
*   `static` pointers -- currently not possible because pointers must be zero-page, thus `@`, thus uninitialized.
*   Question the value of the "consistent initialization" principle for `if` statement analysis.
*   `interrupt` routines -- to indicate that "the supervisor" has stored values on the stack, so we can trash them.
*   Add absolute addressing in shl/shr, absolute-indexed for add, sub, etc.
*   Automatic tail-call optimization (could be tricky, w/constraints?)
*   Possibly `ld x, [ptr] + y`, possibly `st x, [ptr] + y`.
*   Maybe even `copy [ptra] + y, [ptrb] + y`, which can be compiled to indirect LDA then indirect STA!

[VICE]: http://vice-emu.sourceforge.net/
