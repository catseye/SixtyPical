SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with static analysis through abstract interpretation.

In practice, this means it catches things like

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you call trashes a register you thought was preserved

and suchlike.

It is a **work in progress**, currently at the **proof-of-concept** stage.

The current released version of SixtyPical is 0.2.  It is a complete reboot
of SixtyPical 0.1.  The reference implementation is written in Python instead
of Haskell.  The language is much simpler — we're going to try to get the
analysis completely right before adding more sophisticated and useful features
in future versions.

Documentation
-------------

*   Design Goals — coming soon.
*   [SixtyPical specification](doc/SixtyPical.md)
*   [Literate test suite for SixtyPical execution](tests/SixtyPical Execution.md)
*   [Literate test suite for SixtyPical analysis](tests/SixtyPical Analysis.md)
*   [6502 Opcodes used/not used in SixtyPical](doc/6502 Opcodes.md)

TODO
----

For 0.3:

*   extern routines.
*   generate 6502 code for all SixtyPical instructions.
*   a little demo that actually compiles and does something on a C64 emulator.

For 0.4:

*   `while` loops.

For 0.5:

*   add line number (or at least routine name) to error messages.
*   hexadecimal literals.
*   `if not`.
*   6502-mnemonic aliases (`sec`, `clc`)
*   other handy aliases (`eq` for `z`, etc.)
*   source code comments.

For 0.6:

*   `repeat` loops.
*   `word` type.
*   `table` type constructor and indirect addressing.

For 0.7:

*   explicitly-addressed memory locations
