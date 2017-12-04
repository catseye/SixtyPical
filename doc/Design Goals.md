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

### Some Background ###

The ideas in SixtyPical came from a couple of places.

One major impetus was when I was working on [Shelta][], trying to cram
all that code for that compiler into 512 bytes.  This involved looking
at the x86 registers and thinking hard about which ones were preserved
when (and which ones weren't) and making the best use of that.  And
while doing that, one thing that came to mind was: I Bet The Assembler
Could Track This.

Another influence was around 2007 when "Typed Assembly Language" (and
"Proof Carrying Code") were all the rage.  I haven't heard about them
in a while, so I guess they turned out to be research fads?  But for a
while there, it was all Necula, Necula, Necula.  Anyway, I remember at
the time looking into TAL and expecting to find something that matched
the impression I had pre-formulated about what a "Typed Assembly"
might be like.  And finding that it didn't match my vision very well.

I don't actually remember what TAL seemed like to me at the time, but
what I had in mind was more like SixtyPical.

(I'll also write something about abstract interpretation here at some
point, hopefully.)
