SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with block structure and static analysis through abstract interpretation.

It is a work in progress, currently at the proof-of-concept stage.

It is expected that a common use case for SixtyPical would be retroprogramming
for the Commodore 64, VIC-20, Apple ][, etc.

Many SixtyPical instructions map precisely to 6502 opcodes.  However, SixtyPical
is not an assembly language.  The programmer does not have total control over
the layout of code and data in memory.  The language has a type system which
distinguishes addresses from non-addresses (16-bit values for which it does
not make sense to treat them as addresses.)  Some 6502 opcodes have no
SixtyPical equivalent.  Some SixtyPical instructions are named after 6502
opcodes, but generate slightly different (safer, but intuitively related)
sequences of opcodes.  Et cetera.

`sixtypical` is the reference implementation of SixtyPical.  It is written in
Haskell.  It can currently parse and analyze a SixtyPical program, and will
eventually be able to compile it to an Ophis assembler listing.

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

Inside a routine, an address may be declared with `temporary`.  This is like
`static` in C, except the value at that address is not guaranteed to be
retained between invokations of the routine.  Such addresses may only be used
within the routine where they are declared.  If analysis indicates that two
temporary addresses are never used simultaneously, they may be merged
to the same address.

An address knows if it is an address of a byte, of a word, or of a table.

### Blocks ###

Each routine is a block.  It may be composed of inner blocks, attached to
some instructions.

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

Instruction Support so far
--------------------------

A `X` indicates unsupported.  A `!` indicates will-not-support.

Funny syntax indicates use of a special form.

In these, `absolute` must be a `reserve`d or `locate`d address.

    X adc #immediate
    X adc absolute
    
    X and #immediate
    X and absolute
    
    X asl
    X asl absolute
    
      if bcc { block } else { block }
    
      if bcs { block } else { block }
    
      if beq { block } else { block }
    
    X bit absolute
    
      if bmi { block } else { block }
    
      if bne { block } else { block }
    
      if bpl { block } else { block }
    
    ! brk
    
      if bvc { block } else { block }
    
      if bvs { block } else { block }
    
    X clc
    
    X cld
    
    ! cli
    
    X clv
    
      cmp absolute
    X cmp #immediate
      
      cpx absolute
    X cpx #immediate
    
      cpy absolute
    X cpy #immediate
    
      dec absolute
    
      dex
    
      dey
    
    X eor #immediate
    X eor absolute
    
      inc absolute
    
      inx
    
      iny
    
    ! jmp
    
    * jsr routine
    
      lda absolute
    X lda #immediate

      ldx absolute
    X ldx #immediate

      ldy absolute
    X ldy #immediate

    X lsr
    X lsr absolute
    
      nop
      
    X ora #immediate
    X ora absolute

    X pha { block }
    
    X php { block }
    
    ! pla -- (although note this does change flags)
    
    ! plp -- (although note this does change flags -- obviously)
    
    X rol
    X rol absolute
    
    X ror
    X ror absolute
    
    ! rti
    
    ! rts
    
    X sbc #immediate
    X sbc absolute
    
    X sec
    
    X sed
    
    X sei { block }
    
      sta absolute
      
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

*   Parse HEX values like $40A3
*   Tables
*   Character tables ("strings" to everybody else)
*   External routines
*   Work out the analyses again and document them
*   parse support immediate loads, compares
*   number ifs and repeats
*   hello, world sort of program
*   Addressing modes; rename instructions to match

Tests
-----

    -> Tests for functionality "Parse SixtyPical program"

    -> Functionality "Parse SixtyPical program" is implemented by
    -> shell command "bin/sixtypical parse %(test-file)"

    -> Tests for functionality "Check SixtyPical program"
    
    -> Functionality "Check SixtyPical program" is implemented by
    -> shell command "bin/sixtypical check %(test-file)"

`main` must be present.

    | routine main {
    |    nop
    | }
    = True

    | routine frog {
    |    nop
    | }
    ? missing 'main' routine

A program may reserve and assign.

    | reserve word score
    | assign word screen 4000
    | routine main {
    |    lda screen
    |    tax
    |    tay
    |    cmp score
    |    ldx score
    |    txa
    |    ldy score
    |    tya
    | }
    = True

All declarations (`reserve`s and `assign`s) must come before any `routines`.

    | routine main {
    |    lda score
    | }
    | reserve word score
    ? expecting "routine"

All locations used in all routines must be declared first.

    | reserve word score
    | routine main {
    |    lda score
    |    cmp screen
    | }
    ? undeclared location

Even in inner blocks.

    | reserve word score
    | assign word screen 4000
    | routine main {
    |    lda score
    |    cmp screen
    |    if beq {
    |      lda score
    |    } else {
    |      lda fnord
    |    }
    | }
    ? undeclared location

No duplicate declarations.

    | reserve word score
    | assign word score 4000
    | routine main {
    |    nop
    | }
    ? duplicate declaration

    -> Tests for functionality "Emit ASM for SixtyPical program"
    
    -> Functionality "Emit ASM for SixtyPical program" is implemented by
    -> shell command "bin/sixtypical emit %(test-file)"

    | reserve word score
    | assign word screen 4000
    | routine main {
    |    lda screen
    |    inc screen
    |    tax
    |    inx
    |    dex
    |    stx score
    |    tay
    |    iny
    |    dey
    |    sty score
    |    cmp score
    |    ldx score
    |    cpx screen
    |    txa
    |    ldy score
    |    cpy screen
    |    tya
    |    sta screen
    |    dec screen
    | }
    = .org 0
    = .word $0801
    = .org $0801
    = .byte $10, $08, $c9, $07, $9e, $32, $30, $36, $31, $00, $00, $00
    =   jmp main
    = score: .word 0
    = .alias screen 4000
    = main:
    =   lda screen
    =   inc screen
    =   tax
    =   inx
    =   dex
    =   stx score
    =   tay
    =   iny
    =   dey
    =   sty score
    =   cmp score
    =   ldx score
    =   cpx screen
    =   txa
    =   ldy score
    =   cpy screen
    =   tya
    =   sta screen
    =   dec screen
    =   rts

    | assign word screen 4000
    | routine main {
    |    lda screen
    |    cmp screen
    |    if beq {
    |        tax
    |    } else {
    |        tay
    |    }
    |    sta screen
    | }
    = .org 0
    = .word $0801
    = .org $0801
    = .byte $10, $08, $c9, $07, $9e, $32, $30, $36, $31, $00, $00, $00
    =   jmp main
    = .alias screen 4000
    = main:
    =   lda screen
    =   cmp screen
    =   BEQ _label
    =   tay
    =   jmp _past
    = _label:
    =   tax
    = _past:
    =   sta screen
    =   rts

    | assign byte screen 4000
    | reserve byte zero
    | routine main {
    |    ldy zero
    |    repeat bne {
    |       inc screen
    |       dey
    |       cpy zero
    |    }
    |    sty screen
    | }
    = .org 0
    = .word $0801
    = .org $0801
    = .byte $10, $08, $c9, $07, $9e, $32, $30, $36, $31, $00, $00, $00
    =   jmp main
    = .alias screen 4000
    = zero: .byte 0
    = main:
    =   ldy zero
    =   
    = _repeat:
    =   inc screen
    =   dey
    =   cpy zero
    =   BNE _repeat
    =   sty screen
    =   rts
