SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with static analysis through type-checking and abstract interpretation.

It is a **work in progress**, currently at the **proof-of-concept** stage.

It is expected that a common use case for SixtyPical would be retroprogramming
for the Commodore 64 and other 6502-based computers such as the VIC-20.

Many SixtyPical instructions map precisely to 6502 opcodes.  However, SixtyPical
is not an assembly language.  The programmer does not have total control over
the layout of code and data in memory.  The language has a type system which
distinguishes addresses from non-addresses (16-bit values for which it does
not make sense to treat them as addresses.)  Some 6502 opcodes have no
SixtyPical equivalent.  Some SixtyPical instructions are named after 6502
opcodes, but generate slightly different (safer, but intuitively related)
sequences of opcodes.  Et cetera.

`sixtypical` is the reference implementation of SixtyPical.  It is written in
Haskell.  It can currently parse and check a SixtyPical program, and can
emit an Ophis assembler listing for it.

Concepts
--------

### Routines ###

Instead of the assembly-language subroutine, SixtyPical provides the _routine_
as the abstraction for a reusable sequence of code.

A routine may be called, or may be included inline, by another routine.

There is one top-level routine called `main` which represents the entire
program.

The instructions of a routine are analyzed using abstract interpretation.
One thing we specifically do is determine which registers and memory locations
are *not* affected by the routine.

If a register is not affected by a routine, then a caller of that routine may
assume that the value in that register is retained.

Of course, a routine may intentionally affect a register or memory location,
as an output.  It must declare this.  We're not there yet.

### Addresses ###

The body of a routine may not refer to an address literally.  It must use
a symbol that was declared previously.

An address may be declared with `reserve`, which is like `.data` or `.bss`
in an assembler.  This is an address into the program's data.  It is global
to all routines.

An address may be declared with `locate`, which is like `.alias` in an
assembler, with the understanding that the value will be treated "like an
address."  This is generally an address into the operating system or hardware
(e.g. kernal routine, I/O port, etc.)

Not there yet:

> Inside a routine, an address may be declared with `temporary`.  This is like
> `static` in C, except the value at that address is not guaranteed to be
> retained between invokations of the routine.  Such addresses may only be used
> within the routine where they are declared.  If analysis indicates that two
> temporary addresses are never used simultaneously, they may be merged
> to the same address.

An address knows what kind of data is stored at the address:
    
*   `byte`: an 8-bit byte.  not part of a word.  not to be used as an address.
    (could be an index though.)
*   `word`: a 16-bit word.  not to be used as an address.
*   `vector`: a 16-bit address of a routine.  Only a handful of operations
    are supported on vectors:
    
    *   copying the contents of one vector to another
    *   copying the address of a routine into a vector
    *   jumping indirectly to a vector (i.e. to the code at the address
        contained in the vector (and this can only happen at the end of a
        routine (NYI))
    *   `jsr`'ing indirectly to a vector (which is done with a fun
        generated trick (NYI))
    
*   `byte table`: a series of `byte`s contiguous in memory starting from the
    address.  This is the only kind of address that can be used in
    indexed addressing.

### Blocks ###

Each routine is a block.  It may be composed of inner blocks, if those
inner blocks are attached to certain instructions.

SixtyPical does not have instructions that map literally to the 6502 branch
instructions.  Instead, it has an `if` construct, with two blocks (for the
"then" and `else` parts), and the branch instructions map to conditions for
this construct.

Similarly, there is a `repeat` construct.  The same branch instructions can
be used in the condition to this construct.  In this case, they branch back
to the top of the `repeat` loop.

The abstract states of the machine at each of the different block exits are
merged during analysis.  If any register or memory location is treated
inconsistently (e.g. updated in one branch of the test, but not the other,)
that register cannot subsequently be used without a declaration to the effect
that we know what's going on.  (This is all a bit fuzzy right now.)

There is also no `rts` instruction.  It is included at the end of a routine,
but only when the routine is used as a subroutine.  Also, if the routine
ends by `jsr`ing another routine, it reserves the right to do a tail-call
or even a fallthrough.

There are also _with_ instructions, which are associated with an opcode
that has a natural symmetrical opcode (e.g. `pha`, `sei`).  These instructions
take a block.  The natural symmetrical opcode is inserted at the end of the
block.

Unsupported Opcodes
-------------------

6502 opcodes with no language-level equivalent instructions in SixtyPical
are `brk`, `cli`, `pla`, `plp`, `rti`, and `rts`.  These may be
inserted into the output program as a SixtyPical → 6502 compiler sees fit,
however.

Note to self, the `pl` opcodes *do* change flags.

Instruction Support so far
--------------------------

A `X` indicates unsupported.

Funny syntax indicates use of a special form.

In these, `absolute` must be a `reserve`d or `locate`d address.
`immediate` must be a literal decimal or hexadecimal number
(or in future, a declared constant.)

    .
      adc #immediate
      adc absolute
    
      and #immediate
      and absolute
    
    X asl
    X asl absolute
    
      if bcc { block } else { block }
    
      if bcs { block } else { block }
    
      if beq { block } else { block }
    
    X bit absolute
    
      if bmi { block } else { block }
    
      if bne { block } else { block }
    
      if bpl { block } else { block }
    
      if bvc { block } else { block }
    
      if bvs { block } else { block }
    
      clc
    
      cld
    
      clv
    
      cmp #immediate
      cmp absolute
      
      cpx #immediate
      cpx absolute
    
      cpy #immediate
      cpy absolute
    
      dec absolute
    
      dex
    
      dey
    
    X eor #immediate
    X eor absolute
    
      inc absolute
    
      inx
    
      iny
    
    * jsr routine
    X jsr vector
    
    X jmp routine
    * jmp vector
    
      lda #immediate
      lda absolute
      lda absolute, x
      lda absolute, y
      lda (absolute), y

      ldx #immediate
      ldx absolute

      ldy #immediate
      ldy absolute

    X lsr
    X lsr absolute
    
      nop
      
      ora #immediate
      ora absolute

    X pha { block }
    
    X php { block }
    
    X rol
    X rol absolute
    
    X ror
    X ror absolute
    
      sbc #immediate
      sbc absolute
    
      sec
    
      sed
    
      sei { block }
    
      sta absolute
      sta absolute, x
      sta absolute, y
      sta (absolute), y
      
      stx absolute
      
      sty absolute

      tax
      
      tay
      
    X tsx
    
      txa
    
    X txs
    
      tya

TODO
----

*   Initial values for reserved, incl. tables
*   give length for tables, must be there for reserved
*   Character tables ("strings" to everybody else)
*   Work out the analyses again and document them
*   Addressing modes; rename instructions to match
