SixtyPical
==========

_Version 0.12.  Work-in-progress, everything is subject to change._

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

The reference implementation can analyze and compile SixtyPical programs to
6502 machine code.

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

Range-checking buffers might be too difficult.  Range checking tables will be easier.
If a value is ANDed with 15, its range must be 0-15, etc.

### Re-order routines and optimize tail-calls to fallthroughs

Not because it saves 3 bytes, but because it's a neat trick.  Doing it optimally
is probably NP-complete.  But doing it adeuqately is probably not that hard.

### And at some point...

*   `const`s, that can be used in defining the size of tables, etc
*   Remove the need for `forward` and `vector () table` (make grammar changes)
*   Tests, and implementation, ensuring a routine can be assigned to a vector of "wider" type
*   Check that the buffer being read or written to through pointer, appears in approporiate inputs or outputs set.
*   `interrupt` routines -- to indicate that "the supervisor" has stored values on the stack, so we can trash them.
*   error messages that include the line number of the source code
*   add absolute addressing in shl/shr, absolute-indexed for add, sub, etc.
*   automatic tail-call optimization (could be tricky, w/constraints?)
*   possibly `ld x, [ptr] + y`, possibly `st x, [ptr] + y`
*   Maybe even `copy [ptra] + y, [ptrb] + y`, which can be compiled to indirect LDA then indirect STA!

Things it will not do
---------------------

(this will be moved to a FAQ document at some point)

*   Check that a vector is initialized before it's called.
*   Check for recursive calls, or prevent bad things happening because of recursive calls.
    (You can always recursively call yourself through a vector.)
