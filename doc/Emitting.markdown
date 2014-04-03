Emitting Ophis from SixtyPical Programs
=======================================

    -> Tests for functionality "Emit ASM for SixtyPical program"
    
    -> Functionality "Emit ASM for SixtyPical program" is implemented by
    -> shell command "bin/sixtypical emit %(test-file)"

Emitting an `if`.

    | assign byte screen $0400
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

Emitting a `repeat`.

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
    |     copy cinv save_cinv
    |     copy routine our_cinv to cinv
    |   }
    | }
    | 
    | routine our_cinv {
    |   inc screen
    |   jmp (save_cinv)
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

Copy command: immediate -> byte

    | reserve byte position
    | routine main {
    |     copy #23 position
    | }
    = main:
    =   lda #23
    =   sta position
    =   rts
    = 
    = position: .byte 0

Copy command: immediate -> word

    | reserve word position 
    | routine main {
    |     copy #$0400 position
    | }
    = main:
    =   lda #0
    =   sta position
    =   lda #4
    =   sta position+1
    =   rts
    = 
    = position: .word 0
