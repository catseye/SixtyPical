SixtyPical: Instruction Support
===============================

Unsupported Opcodes
-------------------

6502 opcodes with no language-level equivalent instructions in SixtyPical
are `brk`, `cli`, `pla`, `plp`, `rti`, `rts`, `tsx`, `txs`.  These may be
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

    adc #immediate
    adc absolute
    
    and #immediate
    and absolute
    
    asl
    asl absolute
    
    if bcc { block } else { block }
    
    if bcs { block } else { block }
    
    if beq { block } else { block }
    
    bit absolute
    
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
    
    eor #immediate
    eor absolute
    
    inc absolute
    
    inx
    
    iny
    
    jsr routine
    
    jmp (vector)
    
    lda #immediate
    lda absolute
    lda absolute, x
    lda absolute, y
    lda (absolute), y
    
    ldx #immediate
    ldx absolute
    
    ldy #immediate
    ldy absolute
    
    lsr
    lsr absolute
    
    nop
    
    ora #immediate
    ora absolute
    
    pha { block }
    
    php { block }
    
    rol
    rol absolute
    
    ror
    ror absolute
    
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
    
    txa
    
    tya

Tests
-----

Should be merged with the above nicely someday.

    -> Tests for functionality "Emit ASM for SixtyPical program"

Big test for parsing and emitting instructions.

    | reserve word vword
    | reserve byte vbyte
    | assign byte[256] table 1024
    | routine main {
    |    lda #4
    |    ldx #0
    |    ldy #$FF
    |    lda vbyte
    |    lda table, x
    |    lda table, y
    |    lda (vword), y
    |    lda <vword
    |    lda >vword
    |    inc vbyte
    |    tax
    |    inx
    |    dex
    |    stx vbyte
    |    tay
    |    iny
    |    dey
    |    sty vbyte
    |    cmp vbyte
    |    cmp #30
    |    cmp <vword
    |    cmp >vword
    |    ldx vbyte
    |    cpx vbyte
    |    cpx #31
    |    txa
    |    ldy vbyte
    |    cpy vbyte
    |    cpy #32
    |    tya
    |    sta vbyte
    |    sta table, x
    |    sta table, y
    |    sta (vword), y
    |    sta <vword
    |    sta >vword
    |    dec vbyte
    |    clc
    |    cld
    |    clv
    |    sec
    |    sed
    |    adc #8
    |    adc vbyte
    |    and #8
    |    and vbyte
    |    sbc #8
    |    sbc vbyte
    |    ora #8
    |    ora vbyte
    | }
    = main:
    =   lda #4
    =   ldx #0
    =   ldy #255
    =   lda vbyte
    =   lda table, x
    =   lda table, y
    =   lda (vword), y
    =   lda vword
    =   lda vword+1
    =   inc vbyte
    =   tax
    =   inx
    =   dex
    =   stx vbyte
    =   tay
    =   iny
    =   dey
    =   sty vbyte
    =   cmp vbyte
    =   cmp #30
    =   cmp vword
    =   cmp vword+1
    =   ldx vbyte
    =   cpx vbyte
    =   cpx #31
    =   txa
    =   ldy vbyte
    =   cpy vbyte
    =   cpy #32
    =   tya
    =   sta vbyte
    =   sta table, x
    =   sta table, y
    =   sta (vword), y
    =   sta vword
    =   sta vword+1
    =   dec vbyte
    =   clc
    =   cld
    =   clv
    =   sec
    =   sed
    =   adc #8
    =   adc vbyte
    =   and #8
    =   and vbyte
    =   sbc #8
    =   sbc vbyte
    =   ora #8
    =   ora vbyte
    =   rts
    = 
    = .data
    = .space vword 2
    = .space vbyte 1
    = .alias table 1024

    | reserve word vword
    | reserve byte vbyte
    | assign byte[256] table 1024
    | routine main {
    |    asl .a
    |    asl vbyte
    |    lsr .a
    |    lsr vbyte
    |    rol .a
    |    rol vbyte
    |    ror .a
    |    ror vbyte
    |    bit vbyte
    |    eor #5
    |    eor vbyte
    | }
    = main:
    =   asl
    =   asl vbyte
    =   lsr
    =   lsr vbyte
    =   rol
    =   rol vbyte
    =   ror
    =   ror vbyte
    =   bit vbyte
    =   eor #5
    =   eor vbyte
    =   rts
    = 
    = .data
    = .space vword 2
    = .space vbyte 1
    = .alias table 1024

    | routine main {
    |    with pha {
    |        with sei {
    |            with php {
    |                lda #0
    |            }
    |            lda #1
    |        }
    |        lda #2
    |    }
    | }
    = main:
    =   pha
    =   sei
    =   php
    =   lda #0
    =   plp
    =   lda #1
    =   cli
    =   lda #2
    =   pla
    =   rts
