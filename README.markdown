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

Documentation:

*   [doc/SixtyPical.md](SixtyPical.md) — the spec
*   [tests/SixtyPical Execution.md](SixtyPical Execution.md) —
    literate test suite for running SixtyPical programs
*   [tests/SixtyPical Analysis.md](SixtyPical Analysis.md) —
    literate test suite for statically analyzing SixtyPical programs
