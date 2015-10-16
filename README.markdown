SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with static analysis through abstract interpretation.

In practice, this means it catches things like

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you call trashes a register you thought was preserved

and suchlike.

It is a **work in progress**, currently at the **proof-of-concept** stage.

The current version is 0.2-PRE.  It is a complete reboot of SixtyPical 0.1.
The reference implementation is written in Python instead of Haskell.
The language is much simpler — we're going to try to get the analysis
completely right before adding more sophisticated and useful features.

Documentation
-------------

*   Design Goals — coming soon.
*   [doc/SixtyPical.md](SixtyPical.md) — the spec
*   [tests/SixtyPical Execution.md](SixtyPical Execution.md) —
    literate test suite for running SixtyPical programs
*   [tests/SixtyPical Analysis.md](SixtyPical Analysis.md) —
    literate test suite for statically analyzing SixtyPical programs

TODO
----

For 0.2:

*   analyze `if` correctly.

For 0.3:

*   explicitly-addressed memory locations.
*   generate 6502 code (either Ophis assembler or machine code `PRG` files.)
*   `while` loops.
*   `repeat` loops.
*   a little demo that actually compiles and runs on a C64 emulator.

For 0.4 and/or beyond:

*   hexadecimal literals.
*   `word` type.
*   `table` type constructor and indirect addressing.
*   `if not`.
*   6502-mnemonic aliases (`sec`, `clc`)
*   other handy aliases (`eq` for `z`, etc.)
