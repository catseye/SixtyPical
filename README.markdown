SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with static analysis through type-checking and abstract interpretation.

It is a **work in progress**, currently at the **proof-of-concept** stage.

It is expected that a common use case for SixtyPical would be retroprogramming
for the Commodore 64 and other 6502-based computers such as the VIC-20, the
Apple ][+, and the NES.

Many SixtyPical instructions map precisely to 6502 opcodes.  However, SixtyPical
is not an assembly language: the programmer does not have total control over
the layout of code and data in memory.  Some 6502 opcodes have no SixtyPical
equivalent, while some have an equivalent that acts in a slightly different
(but intuitively related) way.  And some commands are unique to SixtyPical.

`sixtypical` is the reference implementation of SixtyPical.  It is written in
Haskell.  It can currently parse and check a SixtyPical program, and can
emit an Ophis assembler listing for it.

SixtyPical itself is distributed under a BSD-style open-source license, while
the example SixtyPical programs in the `eg` directory are in the public domain.
See the file `LICENSE` for more information.

Quick Start
-----------

If you have `ghc`, Ophis, and VICE 2.4 installed, clone this repo, `cd` into it,
and run

    ./loadngo.sh eg/game.60p

The Big Idea(s)
---------------

### Typed Addresses ###

SixtyPical distinguishes several kinds of addresses: those that hold a byte,
those that hold a word (in low-byte-high-byte sequence), those that are the
beginning of a table of bytes, and vectors (those that hold a word pointer to a
machine-language routine.)  It prevents the program from accessing them in
certain ways.  For example, these are illegal:
    
    reserve byte lives
    reserve word score
    routine do_it {
        lda score        // no! can't treat word as if it were a byte
        lda lives, x     // no! can't treat a byte as if it were a table
    }

### Abstract Interpretation ###

SixtyPical tries to prevent the program from using data that has no meaning.

The instructions of a routine are analyzed using abstract interpretation.
One thing we specifically do is determine which registers and memory locations
are *not* affected by the routine.  For example, the following:

    routine do_it {
        lda #0
        jsr update_score
        sta vic_border_colour    // uh... what do we know about reg A here?
    }

...is illegal *unless* one of the following is true:

*   the A register is declared to be a meaningful output of `update_score`
*   `update_score` was analyzed and determined to not change the value of the
    A register

The first case must be done with an explicit declaration on `update_score`.
The second case will be be inferred using abstract interpretation of the code
of `update_score`.

### Structured Programming ###

SixtyPical eschews labels for code and instead organizes code into _blocks_.

Instead of the assembly-language subroutine, SixtyPical provides the _routine_
as the abstraction for a reusable sequence of code.  A routine may be called,
or may be included inline, by another routine.  The body of a routine is a
block.

Along with routines, you get `if`, `repeat`, and `with` constructs which take
blocks.  The `with` construct takes an instruction like `sei` and implicitly
(and unavoidably) inserts the corresponding `cli` at the end of the block.

Abstract interpretation extends to `if` blocks.  The two incoming contexts are
merged, and any storage locations poisoned in either context are considered
poisoned in the result context.  (A similar case applies to `repeat` and
`with`, but these are different too as there is only one block and it is always
executed at least once.)

Declarations can have block scope.  Such declarations may only be used within
the block in which they are declared.  `reserve`d storage inside a block is not,
however, like a local variable (or `auto` in C); rather, it is more like a
`static` in C, except the value at that address is not guaranteed to be
retained between invokations of the block.  This is intended to be used for
temporary storage.  In addition, if analysis of the call graph indicates that
two such temporary addresses are never used simultaneously, they may be merged
to the same address.  (This is, however, not yet implemented, and may not be
implemented for a while.)

### Pseudo-Instructions ###

Along with instructions which map to the 6502 instruction set, SixtyPical
supplies some instructions which are slightly more abstract and powerful.
For lack of a better term, I'm calling them "pseudo-instructions" here.
(But I would really like a better term.)

In a macro assembler, these pseudo-instructions would be implemented with
macros.  However, macros, being textual-substitution-based, are a pain to
analyze.  By providing the functions as built-in instructions, we can
easily work them into the type system.  Also, there are some macros that are
so common and useful that it makes sense for them to be built-ins, with
standardized, prescriptive names.

Such pseudo-instructions are:
    
*   `copy`, which copies a value from one storage location to another.
    This is a typesafe way to copy 16-bit `word`s and `vector`s.
    In the future, it may handle 8-bit values and immediate values too.
*   `save`, which is not yet implemented.  Intended to be used in `with`
    blocks when you want to save a value but you don't want to use the
    stack.  Pairs well with block-level temporary `reserve`d addresses.

### "It's a Partial Solution" ###

SixtyPical does not attempt to force your typed, abstractly interpreted
program to be absolutely watertight.  In assembly language on an 8-bit
microprocessor, you will sometimes _need_ to do dangerous and tricky things,
like self-modifying code and cycle-counting, in order to accomplish a
sophisticated effect, like a raster interrupt trick.

For that reason, `sixtypical` does not attempt to emit a fully-formed
Ophis assembler source.  Instead, it expects you to mix its output with
some raw Ophis assembler to make a complete program.  This "mixin" may contain
as much unchecked assembler code as you like.  An example is provided in the
`lib` directory which adds a prelude that makes the resulting program
runnable from Commodore BASIC 2.0 and stores uninitialized data at `$C000`.

In addition, various checks are not attempted (such as tracking the usage
of an indirect indexed table) and other checks may be subverted (for example
by `assign`ing two variables with two different types of storage at the same
address.)

In summary, SixtyPical helps you write a very-nearly-assembly-level program
which is a bit more "solid" than raw assembly, but it still expects you to
know what you're doing down there.

For More Information
--------------------

For more information, see the docs (which are written in the form of
[Falderal](http://catseye.tc/node/Falderal) literate test suites.  If you
have `falderal` on your executable search path, you can run the tests with
`./test.sh`.)

*   [Checking](https://github.com/catseye/SixtyPical/blob/master/doc/Checking.markdown)
*   [Analyzing](https://github.com/catseye/SixtyPical/blob/master/doc/Analyzing.markdown)
*   [Emitting](https://github.com/catseye/SixtyPical/blob/master/doc/Emitting.markdown)
*   [Instruction Support](https://github.com/catseye/SixtyPical/blob/master/doc/Instruction_Support.markdown)

Internals
---------

Some (OK, a lot) of the Haskell code is kind of gross and non-idiomatic.
The parser, in particular, could not be described as "elegant".  There
could definitely be more higher-order functions defined and used.  At the
same time, I'm really not a fan of pointless style — I prefer it when things
are written out explicitly and pedantically.  Still, there are places where
an added `foldr` or two would not be unwelcome...

The 6502 semantics, which are arguably RISC-like (load/store architecture)
are translated into an intermediate representation which is arguably CISC-like.
For example, `lda`, `sta`, `ldx`, and `tax` all become kinds of `COPY`
internally.  This internal instruction set is much smaller than the 6502's,
and thus is usually easier to analyze.  It would also be easier to adapt to
other instruction sets, such as the Z80 or the 8086.

Notes
-----

This is not quite the right place for this, but I need to write it down
somewhere:

6502 machine code supports an indirect `jmp`, but not an indirect `jsr`.
But an indirect `jsr` is very easy to simulate with an indirect `jmp`.
Instead of

    launch:
        copy whatever to vector
        jsr (vector)
        ...

Just say

    launch:
        copy whatever to vector
        jsr indirect_jsr
        ...
    
    indirect_jsr:
        jmp (vector)

Then the `rts` at the end of your routine pointed to by `vector` will
return you to where you `jsr`ed.

Because the above is so easy to write, SixtyPical will probably not support
a `jsr (vector)` form (unless it would somehow make analysis easier, but
it probably won't.)

TODO
----

*   Addressing modes — indexed mode on more instructions
*   Rename and lift temporaries in nested blocks
*   Tail-recursion optimization
*   `word 100` to promote an otherwise 8-bit literal to a 16-bit value
*   `jmp routine`
*   Enforce that `jmp`s come at ends of blocks(?)
*   `outputs` on externals
*   Routine is a kind of StorageLocation?  (Location)?
*   Test that `pha` restores the A register
*   Test poisonining of flags
*   Test output of flags
