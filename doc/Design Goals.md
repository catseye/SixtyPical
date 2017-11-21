Design Goals for SixtyPical
===========================

(draft)

The intent of SixtyPical is to have a very low-level language that
benefits from abstract interpretation.

"Very low-level" means, on a comparable level of abstraction as
assembly language.

In the original vision for SixtyPical, SixtyPical instructions mapped
nearly 1:1 to 6502 instructions.  However, many times when programming
in 6502 you're using idioms (e.g. adding a 16-bit constant to a 16-bit
value stored in 2 bytes) and it's just massively easier to analyze such
actions when they are represented by a single instruction.

So SixtyPical instructions are similar to, inspired by, and have
analogous restrictions as 6502 instructions, but in many ways, they
are more abstract.  For example, `copy`.

The intent is that programming in SixtyPical is a lot like programming
in 6052 assembler, but it's harder to make a stupid error that you have
to spend a lot of time debugging.

The intent is not to make it absolutely impossible to make such errors,
just harder.
