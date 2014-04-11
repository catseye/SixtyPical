Checking SixtyPical Programs
============================

    -> Tests for functionality "Parse SixtyPical program"

    -> Functionality "Parse SixtyPical program" is implemented by
    -> shell command "bin/sixtypical parse %(test-file)"

    -> Tests for functionality "Check SixtyPical program"
    
    -> Functionality "Check SixtyPical program" is implemented by
    -> shell command "bin/sixtypical check %(test-file)"

Some Basic Syntax
-----------------

`main` must be present.

    | routine main {
    |    nop
    | }
    = True

    | routine frog {
    |    nop
    | }
    ? missing 'main' routine

Each instruction need not appear on its own line.  (Although you probably
still want to write in that style, for consistency with assembly code.)

    | routine main {
    |    nop lda #1 ldx #1 nop
    | }
    = True

Javascript-style block and line comments are both supported.
They may appear anywhere whitespace may appear.

    | reserve byte lives      /* fnord */
    | assign byte gdcol 647   // fnord
    | external blastoff 4     // fnnnnnnnnnnnnnnnnfffffffff
    | 
    | routine /* hello */ main {
    |    /* this routine does everything you need. */
    |    lda #1   // we assemble the fnord using
    |    ldx #1   // multiple lorem ipsums which
    |    ldy #1
    |    lda #1   /* we
    |                found under the bridge by the old mill yesterday */
    |    ldx #1
    | }
    = True

Addresses
---------

An address may be declared with `reserve`, which is like `.data` or `.bss`
in an assembler.  This is an address into the program's data.  It is global
to all routines.

    | reserve byte lives
    | routine main {
    |    lda #3
    |    sta lives
    | }
    | routine died {
    |    dec lives
    | }
    = True

An address declared with `reserve` may be given an initial value.

    | reserve byte lives : 3
    | routine main {
    |    sta lives
    | }
    | routine died {
    |    dec lives
    | }
    = True

An address may be declared with `locate`, which is like `.alias` in an
assembler, with the understanding that the value will be treated "like an
address."  This is generally an address into the operating system or hardware
(e.g. kernal routine, I/O port, etc.)

    | assign byte screen $0400
    | routine main {
    |    lda #0
    |    sta screen
    | }
    = True

The body of a routine may not refer to an address literally.  It must use
a symbol that was declared previously with `reserve` or `assign`.

    | routine main {
    |    lda #0
    |    sta $0400
    | }
    ? unexpected "$"

    | assign byte screen $0400
    | routine main {
    |    lda #0
    |    sta screen
    | }
    = True

Test for many combinations of `reserve` and `assign`.

    | reserve byte lives
    | assign byte gdcol 647
    | reserve word score
    | assign word memstr 641
    | reserve vector v
    | assign vector cinv 788
    | reserve byte[16] frequencies
    | assign byte[256] screen 1024
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

    | reserve byte score
    | routine main {
    |    lda score
    |    cmp screen
    | }
    ? undeclared location

Even in inner blocks.

    | reserve byte score
    | assign byte screen 1024
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

We can jump indirectly through a vector.

    | reserve vector blah
    | routine main {
    |    jmp (blah)
    | }
    = True

We can't jump indirectly through a word.

    | reserve word blah
    | routine main {
    |    jmp (blah)
    | }
    ? jmp to non-vector

We can't jump indirectly through a byte.

    | assign byte screen 1024
    | routine main {
    |    jmp (screen)
    | }
    ? jmp to non-vector

We can absolute-indexed a byte table.

    | assign byte[256] screen 1024
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

We cannot absolute access a word.

    | assign word screen 1024
    | routine main {
    |    ldx screen
    | }
    ? incompatible types 'Word' and 'Byte'

No, not even with `ora`.

    | assign word screen 1024
    | routine main {
    |    ora screen
    | }
    ? incompatible types 'Byte' and 'Word'

Instead, we have to do this.

    | assign word screen 1024
    | routine main {
    |    lda <screen
    |    lda >screen
    | }
    = True

We cannot absolute access a vector.

    | assign vector screen 1024
    | routine main {
    |    lda screen
    | }
    ? incompatible types 'Vector' and 'Byte'

### Addresses ###

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
    
*   `byte [SIZE]`: a series of `SIZE` `byte`s contiguous in memory starting
    from the address.  This is the only kind of address that can be used in
    indexed addressing.  `SIZE` has a minimum of 1 and a maximum of 256.

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

There are also _with_ instructions, which are associated with three opcodes
that have natural symmetrical opcodes: `pha`, `php`, and `sei`.  These
instructions take a block.  The natural symmetrical opcode is inserted at
the end of the block.
