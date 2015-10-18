SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with static analysis through abstract interpretation.

In practice, this means it catches things like

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you call trashes a register you thought was preserved

and suchlike.

It is a **work in progress**, currently at the **proof-of-concept** stage.

The current released version of SixtyPical is 0.3.  The current development
version of SixtyPical, unreleased as of this writing, is 0.4-PRE.

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

For 0.4:

*   explicitly-addressed memory locations

For 0.5:

*   add line number (or at least routine name) to error messages.
*   hexadecimal literals.
*   6502-mnemonic aliases (`sec`, `clc`)
*   other handy aliases (`eq` for `z`, etc.)
*   source code comments.

For 0.6:

*   `word` type.
*   `table` type constructor and indirect addressing.
