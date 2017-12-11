SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with static analysis through abstract interpretation.

In practice, this means it catches things like

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you call trashes a register you thought was preserved
*   you tried to write the address of something that was not a routine, to
    a jump vector

and suchlike.  It also provides some convenient operations and abstractions
based on common machine-language programming idioms, such as

*   copying values from one register to another (via a third register when
    there are no underlying instructions that directly support it)
*   explicit tail calls
*   indirect subroutine calls

The reference implementation can execute, analyze, and compile SixtyPical
programs to 6502 machine code.

SixtyPical is a work in progress.  The current released version of SixtyPical
is 0.9-PRE (not released yet.)

Documentation
-------------

*   [Design Goals](doc/Design%20Goals.md)
*   [SixtyPical specification](doc/SixtyPical.md)
*   [SixtyPical revision history](HISTORY.md)
*   [Literate test suite for SixtyPical syntax](tests/SixtyPical%20Syntax.md)
*   [Literate test suite for SixtyPical execution](tests/SixtyPical%20Execution.md)
*   [Literate test suite for SixtyPical analysis](tests/SixtyPical%20Analysis.md)
*   [Literate test suite for SixtyPical compilation](tests/SixtyPical%20Compilation.md)
*   [6502 Opcodes used/not used in SixtyPical](doc/6502%20Opcodes.md)

TODO
----

Finish the little demo "game" where you can move a block around the screen with
the joystick (i.e. bring it up to par with the original demo game that was written
for SixtyPical)

### Operations on 16 bit values

Compare word (constant or memory location) with memory location or pointer.  (Maybe?)

### `vector table` type

### `low` and `high` address operators

To turn `word` type into `byte`.

### Save registers on stack

This preserves them, so that, semantically, they can be used later even though they
are trashed inside the block.

### Range checking in the abstract interpretation

If you copy the address of a buffer (say it is size N) to a pointer, it is valid.
If you add a value from 0 to N-1 to the pointer, it is still valid.
But if you add a value ≥ N to it, it becomes invalid.
This should be tracked in the abstract interpretation.
(If only because abstract interpretation is the major point of this project!)

### And at some point...

*   `copy x, [ptr] + y`
*   Maybe even `copy [ptra] + y, [ptrb] + y`, which can be compiled to indirect LDA then indirect STA!
*   Check that the buffer being read or written to through pointer, appears in approporiate inputs or outputs set.
*   `byte table` and `word table` of sizes other than 256
*   initialized `byte table` memory locations
*   always analyze before executing or compiling, unless told not to
*   `trash` instruction.
*   `interrupt` routines -- to indicate that "the supervisor" has stored values on the stack, so we can trash them.
*   pre-initialized `word` variables
*   error messages that include the line number of the source code
*   have `copy` instruction able to copy a byte to a user-def mem loc, etc.
*   add absolute addressing in shl/shr, absolute-indexed for add, sub, etc.
*   check and disallow recursion.
*   automatic tail-call optimization (could be tricky, w/constraints?)
*   re-order routines and optimize tail-calls to fallthroughs
