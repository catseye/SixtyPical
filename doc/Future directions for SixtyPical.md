Future directions for SixtyPical
================================

[SixtyPical](https://codeberg.org/catseye/SixtyPical) has reached a mature
stage of development.  There are small features that could be added, but
they are minor compared to the main features
(abstract-interpretation/symbolic-execution/flow-typing-based
static analysis and optimization of low-level 6502 programs).
So the question arises -- what would be next for SixtyPical?

It would be nice to port SixtyPical to work on a ISA other than the 6502 --
the Z80, for example.  Or, a more practical choice would be the X86
architecture.

It would also be nice if it could somehow be made to work on regular
assembly language programs.  It would still be acceptable if, in this
case, it only worked on programs written in a particular style, to be
compatible with SixtyPical (structured "for" loops, no arbitrary jumps,
and so forth).

It would also be nice to simply generalize the idea: a generic
low-level language with a generic set of registers
(global variables), the rules for which we can specify as part of
the program.  The static analyzer then checks the program according to the rules.

It would also be nice to design a more formal theory behind all
this.  While SixtyPical works well for what it does, much of it was
"unit-tested into existence" and the theories behind these parts are not made explicit.

In fact, I think we can combine all these ideas.  (And, if these are
really all nice to have in a next version of SixtyPical -- we should.)

We can do this by splitting it up into a few different phases:

The first phase is a "frontend" that takes a 6502 assembly language program in a common
6502 assembly language format (perhaps with annotations, or perhaps accompanied by a
configuration file) and translated it into a program description in the generic language,
including the specification of the rules particular to the 6502.

There is then a generic analyzer which checks the program in the
generic language.

If all the checks pass, then our guarantees have been met and the
original assembly language program can simply be assembled, using any
assembler that can understand its format.  (This approach is similar
to using a model checker.)

Unlike SixtyPical currently,
which is an optimizing translator, in this method there would be no
optimizations applied.  But I think this is an acceptable trade-off.
It is especially acceptable if the assembly language input can be hand-optimized
and still be checked by the analyzer, but this would be trickier to accomplish.

Then, to support other architectures, one could define a similar "frontend"
which works on Z80 assembly code, or X86 assembly, or what have you.

In practice, it would probably be easiest to start with a
"frontend" which converts 6502 assembly to the existing SixtyPical
language.  Then design and implement the generic language.
Then re-target the frontend to the generic language.
