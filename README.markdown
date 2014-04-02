SixtyPical
==========

SixtyPical is a very low-level programming language, similar to 6502 assembly,
with block structure and static analysis through abstract interpretation.

It is a work in progress, currently at the proof-of-concept stage.

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

A `X` indicates unsupported.  A `!` indicates will-not-support.

Funny syntax indicates use of a special form.

In these, `absolute` must be a `reserve`d or `locate`d address.

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
*   lda wordaddress --> is not legal.  use lda <wordaddr or lda >wordaddr
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

A comment may appear at the start of a block.

    | routine main {
    |    ; this program does nothing
    |    nop
    | }
    = True

A comment may appear after each command.

    | routine main {
    |    lda #1   ; we assemble the fnord using
    |    ldx #1   ; multiple lorem ipsums which
    |    ldy #1
    |    lda #1   ; we
    |    ldx #1   ; found under the bridge by the old mill yesterday
    | }
    = True

A comment may appear after each declaration.

    | reserve byte lives      ; fnord
    | assign byte gdcol 647   ; fnord
    | external blastoff 4     ; fnnnnnnnnnnnnnnnnfffffffff
    | 
    | routine main {
    |   nop
    | }
    = True

A program may `reserve` and `assign`.

    | reserve byte lives
    | assign byte gdcol 647
    | reserve word score
    | assign word memstr 641
    | reserve vector v
    | assign vector cinv 788
    | reserve byte table frequencies
    | assign byte table screen 1024
    | routine main {
    |    nop
    | }
    = True

A program may declare an `external`.

    | external blastoff 49152
    | routine main {
    |    jsr blastoff
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
    | assign word screen 1024
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

All routines jsr'ed to must be defined, or external.

    | routine main {
    |    jsr blastoff
    | }
    ? undeclared routine

No duplicate location names in declarations.

    | reserve word score
    | assign word score 4000
    | routine main {
    |    nop
    | }
    ? duplicate location name

No duplicate routine names.

    | routine main {
    |    nop
    | }
    | routine main {
    |    txa
    | }
    ? duplicate routine name

No duplicate routine names, including externals.

    | external main 7000
    | routine main {
    |    nop
    | }
    ? duplicate routine name

We can jump to a vector.

    | reserve vector blah
    | routine main {
    |    jmp blah
    | }
    = True

We can't jump to a word.

    | reserve word blah
    | routine main {
    |    jmp blah
    | }
    ? jmp to non-vector

We can't jump to a byte.

    | assign byte screen 1024
    | routine main {
    |    jmp screen
    | }
    ? jmp to non-vector

We can absolute-indexed a byte table.

    | assign byte table screen 1024
    | routine main {
    |    sta screen, x
    | }
    = True

We cannot absolute-indexed a byte.

    | assign byte screen 1024
    | routine main {
    |    sta screen, x
    | }
    ? indexed access of non-table

We cannot absolute-indexed a word.

    | assign word screen 1024
    | routine main {
    |    sta screen, x
    | }
    ? indexed access of non-table

    -> Tests for functionality "Emit ASM for SixtyPical program"
    
    -> Functionality "Emit ASM for SixtyPical program" is implemented by
    -> shell command "bin/sixtypical emit %(test-file)"

    | reserve word score
    | assign byte table screen 1024
    | routine main {
    |    lda #4
    |    ldx #0
    |    ldy #$FF
    |    lda screen
    |    lda screen, x
    |    lda screen, y
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
    |    cmp #30
    |    ldx score
    |    cpx screen
    |    cpx #31
    |    txa
    |    ldy score
    |    cpy screen
    |    cpy #32
    |    tya
    |    sta screen
    |    sta screen, x
    |    sta screen, y
    |    sta (screen), y
    |    dec screen
    |    clc
    |    cld
    |    clv
    |    sec
    |    sed
    |    adc #8
    |    adc screen
    |    and #8
    |    and screen
    |    sbc #8
    |    sbc screen
    |    ora #8
    |    ora screen
    | }
    = main:
    =   lda #4
    =   ldx #0
    =   ldy #255
    =   lda screen
    =   lda screen, x
    =   lda screen, y
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
    =   cmp #30
    =   ldx score
    =   cpx screen
    =   cpx #31
    =   txa
    =   ldy score
    =   cpy screen
    =   cpy #32
    =   tya
    =   sta screen
    =   sta screen, x
    =   sta screen, y
    =   sta (screen), y
    =   dec screen
    =   clc
    =   cld
    =   clv
    =   sec
    =   sed
    =   adc #8
    =   adc screen
    =   and #8
    =   and screen
    =   sbc #8
    =   sbc screen
    =   ora #8
    =   ora screen
    =   rts
    = 
    = score: .word 0
    = .alias screen 1024

    | assign word screen $0400
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
    = main:
    =   lda screen
    =   cmp screen
    =   BEQ _label_1
    =   tay
    =   jmp _past_1
    = _label_1:
    =   tax
    = _past_1:
    =   sta screen
    =   rts
    = 
    = .alias screen 1024

    | assign byte screen 1024
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
    = main:
    =   ldy zero
    =   
    = _repeat_1:
    =   inc screen
    =   dey
    =   cpy zero
    =   BNE _repeat_1
    =   sty screen
    =   rts
    = 
    = .alias screen 1024
    = zero: .byte 0

Nested ifs.

    | routine main {
    |   if beq {
    |     if bcc {
    |       lda #0
    |     } else {
    |       if bvs {
    |         lda #1
    |       } else {
    |         lda #2
    |       }
    |     }
    |   } else {
    |     lda #3
    |   }
    | }
    = main:
    =   BEQ _label_3
    =   lda #3
    =   jmp _past_3
    = _label_3:
    =   BCC _label_2
    =   BVS _label_1
    =   lda #2
    =   jmp _past_1
    = _label_1:
    =   lda #1
    = _past_1:
    =   jmp _past_2
    = _label_2:
    =   lda #0
    = _past_2:
    = _past_3:
    =   rts

Installing an interrupt handler (at the Kernal level, i.e. with CINV)

    | assign byte screen 1024
    | assign vector cinv 788
    | reserve vector save_cinv
    | 
    | routine main {
    |   sei {
    |     copy vector cinv to save_cinv
    |     copy routine our_cinv to cinv
    |   }
    | }
    | 
    | routine our_cinv {
    |   inc screen
    |   jmp save_cinv
    | }
    = main:
    =   sei
    =   lda cinv
    =   sta save_cinv
    =   lda cinv+1
    =   sta save_cinv+1
    =   lda #<our_cinv
    =   sta cinv
    =   lda #>our_cinv
    =   sta cinv+1
    =   cli
    =   rts
    = 
    = our_cinv:
    =   inc screen
    =   jmp (save_cinv)
    =   rts
    = 
    = .alias screen 1024
    = .alias cinv 788
    = save_cinv: .word 0
