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

It is a **work in progress**, currently at the **proof-of-concept** stage.

The current development version of SixtyPical is 0.8-PRE.

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

*   `word table` type.
*   `vector table` type.
*   zero-page memory locations.
*   indirect addressing.
*   `low` and `high` address operators (turn `word` type into `byte`.)  Possibly.
*   save registers on stack or in memory (this preserves them = not trashed)

At some point...

*   initialized `byte table` memory locations
*   always analyze before executing or compiling, unless told not to
*   `trash` instruction.
*   `interrupt` routines.
*   6502-mnemonic aliases (`sec`, `clc`)
*   other handy aliases (`eq` for `z`, etc.)
*   have `copy` instruction able to copy a constant to a user-def mem loc, etc.
*   add absolute addressing in shl/shr, absolute-indexed for add, sub, etc.
*   check and disallow recursion.
*   automatic tail-call optimization (could be tricky, w/constraints?)
*   re-order routines and optimize tail-calls to fallthroughs
