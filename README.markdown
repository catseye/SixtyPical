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

The current released version of SixtyPical is 0.6.  The current development
version of SixtyPical, unreleased as of this writing, is 0.7-PRE.

Documentation
-------------

*   Design Goals — coming soon.
*   [SixtyPical specification](doc/SixtyPical.md)
*   [SixtyPical history](HISTORY.md)
*   [Literate test suite for SixtyPical syntax](tests/SixtyPical Syntax.md)
*   [Literate test suite for SixtyPical execution](tests/SixtyPical Execution.md)
*   [Literate test suite for SixtyPical analysis](tests/SixtyPical Analysis.md)
*   [Literate test suite for SixtyPical compilation](tests/SixtyPical Compilation.md)
*   [6502 Opcodes used/not used in SixtyPical](doc/6502 Opcodes.md)

TODO
----

For 0.7:

*   `word` type.
*   `word table` type.

For 0.8:

*   zero-page memory locations.
*   indirect addressing.

For 0.9

*   save registers on stack or in memory (the preserves them = not trashed)

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
