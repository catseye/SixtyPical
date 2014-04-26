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
    = .data
    = .alias screen 1024

Emitting a `repeat`.

    | assign byte screen 1024
    | reserve byte four : $04
    | routine main {
    |    ldy four
    |    repeat bne {
    |       inc screen
    |       dey
    |       cpy four
    |    }
    |    sty screen
    | }
    = main:
    =   ldy four
    =   
    = _repeat_1:
    =   inc screen
    =   dey
    =   cpy four
    =   BNE _repeat_1
    =   sty screen
    =   rts
    = 
    = four: .byte 4
    = .data
    = .alias screen 1024

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
    |   with sei {
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
    = .data
    = .alias screen 1024
    = .alias cinv 788
    = .space save_cinv 2

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
    = .data
    = .space position 1

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
    = .data
    = .space position 2

Copy command: byte-sized immediate -> word

Disabled for now.

        | reserve word position
        | routine main {
        |     copy #1 position
        | }
        = main:
        =   lda #1
        =   sta position
        =   lda #0
        =   sta position+1
        =   rts
        = 
        = .data
        = .space position 2

Copy command: word -> word

    | reserve word position1
    | reserve word position2
    | routine main {
    |     copy position1 position2
    | }
    = main:
    =   lda position1
    =   sta position2
    =   lda position1+1
    =   sta position2+1
    =   rts
    = 
    = .data
    = .space position1 2
    = .space position2 2

Copy command: word -> word indexed

    | reserve word loc
    | reserve word[4] locs
    | routine main {
    |     ldy #0
    |     copy loc locs, y
    | }
    = main:
    =   ldy #0
    =   lda loc
    =   sta locs_lo, y
    =   lda loc+1
    =   sta locs_hi, y
    =   rts
    = 
    = .data
    = .space loc 2
    = .space locs_lo 4
    = .space locs_hi 4

Copy command: word INDEXED -> word

    | reserve word loc
    | reserve word[4] locs
    | routine main {
    |     ldx #0
    |     copy locs, x loc
    | }
    = main:
    =   ldx #0
    =   lda locs_lo, x
    =   sta loc
    =   lda locs_hi, x
    =   sta loc+1
    =   rts
    = 
    = .data
    = .space loc 2
    = .space locs_lo 4
    = .space locs_hi 4

Copy command: byte -> indexed word table -> error.

    | reserve byte bbb
    | reserve word[4] locs
    | routine main {
    |     ldx #0
    |     copy bbb locs, x
    | }
    ? incompatible types 'Byte' and 'Table Word 4'

Copy command: byte -> low byte of indexed word table

    | reserve byte bbb
    | reserve word[4] locs
    | routine main {
    |     ldx #0
    |     copy bbb <locs, x
    | }
    = main:
    =   ldx #0
    =   lda bbb
    =   sta locs_lo, x
    =   rts
    = 
    = .data
    = .space bbb 1
    = .space locs_lo 4
    = .space locs_hi 4

Copy command: byte -> high byte of indexed word table

    | reserve byte bbb
    | reserve word[4] locs
    | routine main {
    |     ldx #0
    |     copy bbb >locs, x
    | }
    = main:
    =   ldx #0
    =   lda bbb
    =   sta locs_hi, x
    =   rts
    = 
    = .data
    = .space bbb 1
    = .space locs_lo 4
    = .space locs_hi 4

Copy command: low byte of indexed word table -> byte

    | reserve byte bbb
    | reserve word[4] locs
    | routine main {
    |     ldx #0
    |     copy <locs, x bbb
    | }
    = main:
    =   ldx #0
    =   lda locs_lo, x
    =   sta bbb
    =   rts
    = 
    = .data
    = .space bbb 1
    = .space locs_lo 4
    = .space locs_hi 4

Copy command: high byte of indexed word table -> byte

    | reserve byte bbb
    | reserve word[4] locs
    | routine main {
    |     ldx #0
    |     copy >locs, x bbb
    | }
    = main:
    =   ldx #0
    =   lda locs_hi, x
    =   sta bbb
    =   rts
    = 
    = .data
    = .space bbb 1
    = .space locs_lo 4
    = .space locs_hi 4

`main` is always emitted first.

    | reserve word position 
    | routine foo {
    |     inx
    | }
    | routine main {
    |     jsr foo
    |     jsr foo
    | }
    = main:
    =   jsr foo
    =   jsr foo
    =   rts
    = 
    = foo:
    =   inx
    =   rts
    = 
    = .data
    = .space position 2

Reserving and assigning byte tables.

    | reserve byte[16] frequencies
    | assign byte[256] screen $0400
    | routine main {
    |     lda #0
    |     ldy #0
    |     sta frequencies, y
    |     sta screen, y
    | }
    = main:
    =   lda #0
    =   ldy #0
    =   sta frequencies, y
    =   sta screen, y
    =   rts
    = 
    = .data
    = .space frequencies 16
    = .alias screen 1024

Reserving things with initial values.

    | reserve byte lives : 3
    | reserve word screen : $0400
    | reserve byte[8] frequencies : (0 1 2 4 5 8 9 10)
    | reserve byte[13] message : "Hello, world!"
    | routine main {
    | }
    = main:
    =   rts
    = 
    = lives: .byte 3
    = screen: .word 1024
    = frequencies: .byte 0, 1, 2, 4, 5, 8, 9, 10
    = message: .byte 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33

Temporary storage, in the form of block-local declarations.  Note that these
temporaries are not unioned yet, but they could be.

    | routine a {
    |     reserve byte foo
    |     reserve word bar
    |     lda foo
    |     sta >bar
    | }
    | routine b {
    |     reserve byte baz
    |     reserve word quuz
    |     lda baz
    |     sta <quuz
    | }
    | routine main {
    |     jsr a
    |     jsr b
    | }
    = main:
    =   jsr a
    =   jsr b
    =   rts
    = 
    = a:
    =   lda _temp_1
    =   sta _temp_2+1
    =   rts
    = 
    = b:
    =   lda _temp_3
    =   sta _temp_4
    =   rts
    = 
    = .data
    = .space _temp_3 1
    = .space _temp_4 2
    = .space _temp_1 1
    = .space _temp_2 2

Declaring and calling an external routine.

    | external chrout 65490
    | routine main {
    |     lda #72
    |     jsr chrout
    |     lda #73
    |     jsr chrout
    |     lda #13
    |     jsr chrout
    | }
    = main:
    =   lda #72
    =   jsr chrout
    =   lda #73
    =   jsr chrout
    =   lda #13
    =   jsr chrout
    =   rts
    = 
    = .data
    = .alias chrout 65490

