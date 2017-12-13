SixtyPical Compilation
======================

This is a test suite, written in [Falderal][] format, for compiling
SixtyPical to 6502 machine code.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Compile SixtyPical program" is implemented by
    -> shell command "bin/sixtypical --basic-prelude --compile %(test-body-file) | tests/appliances/bin/dcc6502-adapter"

    -> Tests for functionality "Compile SixtyPical program"

Null program.

    | routine main
    | {
    | }
    = $080D   RTS

Rudimentary program.

    | routine main
    |   inputs a
    |   outputs a
    |   trashes c, z, n, v
    | {
    |     st off, c
    |     add a, 4
    | }
    = $080D   CLC
    = $080E   ADC #$04
    = $0810   RTS

Call extern.

    | routine chrout
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | routine main
    |   inputs a
    |   trashes a, z, n
    | {
    |     ld a, 65
    |     call chrout
    | }
    = $080D   LDA #$41
    = $080F   JSR $FFD2
    = $0812   RTS

Call defined routine.

    | routine foo
    |   outputs a, x, y
    |   trashes z, n
    | {
    |   ld a, 0
    |   ld x, 0
    |   ld y, 0
    | }
    | 
    | routine main
    |   trashes a, x, y, z, n
    | {
    |     call foo
    | }
    = $080D   JSR $0811
    = $0810   RTS
    = $0811   LDA #$00
    = $0813   LDX #$00
    = $0815   LDY #$00
    = $0817   RTS

Access a defined memory location.

    | byte foo
    | 
    | routine main
    |   trashes a, y, z, n, foo
    | {
    |     ld y, 0
    |     st y, foo
    |     ld a, foo
    | }
    = $080D   LDY #$00
    = $080F   STY $0816
    = $0812   LDA $0816
    = $0815   RTS

Memory location with explicit address.

    | byte screen @ 1024
    | 
    | routine main
    |   trashes a, z, n, screen
    | {
    |   ld a, 100
    |   st a, screen
    | }
    = $080D   LDA #$64
    = $080F   STA $0400
    = $0812   RTS

Memory location with initial value.

    | byte lives : 3
    | 
    | routine main
    |   inputs lives
    |   trashes a, z, n
    | {
    |   ld a, lives
    | }
    = $080D   LDA $0811
    = $0810   RTS
    = $0811   .byte $03

Word memory locations with explicit address, initial value.

    | word w1 @ 60001
    | word w2 : 3003
    | 
    | routine main
    |   inputs w1
    |   outputs w2
    |   trashes a, z, n
    | {
    |   copy w1, w2
    | }
    = $080D   LDA $EA61
    = $0810   STA $081A
    = $0813   LDA $EA62
    = $0816   STA $081B
    = $0819   RTS
    = $081A   .byte $BB
    = $081B   .byte $0B

Initialized byte table.

    | byte table message : "WHAT?"
    | 
    | routine main
    |   inputs message
    |   outputs x, a, z, n
    | {
    |   ld x, 0
    |   ld a, message + x
    | }
    = $080D   LDX #$00
    = $080F   LDA $0813,X
    = $0812   RTS
    = $0813   .byte $57
    = $0814   PHA
    = $0815   EOR ($54,X)
    = $0817   .byte $3F
    = $0818   BRK
    = $0819   BRK
    = $081A   BRK
    = $081B   BRK
    = $081C   BRK
    = $081D   BRK
    = $081E   BRK
    = $081F   BRK
    = $0820   BRK
    = $0821   BRK
    = $0822   BRK
    = $0823   BRK
    = $0824   BRK
    = $0825   BRK
    = $0826   BRK
    = $0827   BRK
    = $0828   BRK
    = $0829   BRK
    = $082A   BRK
    = $082B   BRK
    = $082C   BRK
    = $082D   BRK
    = $082E   BRK
    = $082F   BRK
    = $0830   BRK
    = $0831   BRK
    = $0832   BRK
    = $0833   BRK
    = $0834   BRK
    = $0835   BRK
    = $0836   BRK
    = $0837   BRK
    = $0838   BRK
    = $0839   BRK
    = $083A   BRK
    = $083B   BRK
    = $083C   BRK
    = $083D   BRK
    = $083E   BRK
    = $083F   BRK
    = $0840   BRK
    = $0841   BRK
    = $0842   BRK
    = $0843   BRK
    = $0844   BRK
    = $0845   BRK
    = $0846   BRK
    = $0847   BRK
    = $0848   BRK
    = $0849   BRK
    = $084A   BRK
    = $084B   BRK
    = $084C   BRK
    = $084D   BRK
    = $084E   BRK
    = $084F   BRK
    = $0850   BRK
    = $0851   BRK
    = $0852   BRK
    = $0853   BRK
    = $0854   BRK
    = $0855   BRK
    = $0856   BRK
    = $0857   BRK
    = $0858   BRK
    = $0859   BRK
    = $085A   BRK
    = $085B   BRK
    = $085C   BRK
    = $085D   BRK
    = $085E   BRK
    = $085F   BRK
    = $0860   BRK
    = $0861   BRK
    = $0862   BRK
    = $0863   BRK
    = $0864   BRK
    = $0865   BRK
    = $0866   BRK
    = $0867   BRK
    = $0868   BRK
    = $0869   BRK
    = $086A   BRK
    = $086B   BRK
    = $086C   BRK
    = $086D   BRK
    = $086E   BRK
    = $086F   BRK
    = $0870   BRK
    = $0871   BRK
    = $0872   BRK
    = $0873   BRK
    = $0874   BRK
    = $0875   BRK
    = $0876   BRK
    = $0877   BRK
    = $0878   BRK
    = $0879   BRK
    = $087A   BRK
    = $087B   BRK
    = $087C   BRK
    = $087D   BRK
    = $087E   BRK
    = $087F   BRK
    = $0880   BRK
    = $0881   BRK
    = $0882   BRK
    = $0883   BRK
    = $0884   BRK
    = $0885   BRK
    = $0886   BRK
    = $0887   BRK
    = $0888   BRK
    = $0889   BRK
    = $088A   BRK
    = $088B   BRK
    = $088C   BRK
    = $088D   BRK
    = $088E   BRK
    = $088F   BRK
    = $0890   BRK
    = $0891   BRK
    = $0892   BRK
    = $0893   BRK
    = $0894   BRK
    = $0895   BRK
    = $0896   BRK
    = $0897   BRK
    = $0898   BRK
    = $0899   BRK
    = $089A   BRK
    = $089B   BRK
    = $089C   BRK
    = $089D   BRK
    = $089E   BRK
    = $089F   BRK
    = $08A0   BRK
    = $08A1   BRK
    = $08A2   BRK
    = $08A3   BRK
    = $08A4   BRK
    = $08A5   BRK
    = $08A6   BRK
    = $08A7   BRK
    = $08A8   BRK
    = $08A9   BRK
    = $08AA   BRK
    = $08AB   BRK
    = $08AC   BRK
    = $08AD   BRK
    = $08AE   BRK
    = $08AF   BRK
    = $08B0   BRK
    = $08B1   BRK
    = $08B2   BRK
    = $08B3   BRK
    = $08B4   BRK
    = $08B5   BRK
    = $08B6   BRK
    = $08B7   BRK
    = $08B8   BRK
    = $08B9   BRK
    = $08BA   BRK
    = $08BB   BRK
    = $08BC   BRK
    = $08BD   BRK
    = $08BE   BRK
    = $08BF   BRK
    = $08C0   BRK
    = $08C1   BRK
    = $08C2   BRK
    = $08C3   BRK
    = $08C4   BRK
    = $08C5   BRK
    = $08C6   BRK
    = $08C7   BRK
    = $08C8   BRK
    = $08C9   BRK
    = $08CA   BRK
    = $08CB   BRK
    = $08CC   BRK
    = $08CD   BRK
    = $08CE   BRK
    = $08CF   BRK
    = $08D0   BRK
    = $08D1   BRK
    = $08D2   BRK
    = $08D3   BRK
    = $08D4   BRK
    = $08D5   BRK
    = $08D6   BRK
    = $08D7   BRK
    = $08D8   BRK
    = $08D9   BRK
    = $08DA   BRK
    = $08DB   BRK
    = $08DC   BRK
    = $08DD   BRK
    = $08DE   BRK
    = $08DF   BRK
    = $08E0   BRK
    = $08E1   BRK
    = $08E2   BRK
    = $08E3   BRK
    = $08E4   BRK
    = $08E5   BRK
    = $08E6   BRK
    = $08E7   BRK
    = $08E8   BRK
    = $08E9   BRK
    = $08EA   BRK
    = $08EB   BRK
    = $08EC   BRK
    = $08ED   BRK
    = $08EE   BRK
    = $08EF   BRK
    = $08F0   BRK
    = $08F1   BRK
    = $08F2   BRK
    = $08F3   BRK
    = $08F4   BRK
    = $08F5   BRK
    = $08F6   BRK
    = $08F7   BRK
    = $08F8   BRK
    = $08F9   BRK
    = $08FA   BRK
    = $08FB   BRK
    = $08FC   BRK
    = $08FD   BRK
    = $08FE   BRK
    = $08FF   BRK
    = $0900   BRK
    = $0901   BRK
    = $0902   BRK
    = $0903   BRK
    = $0904   BRK
    = $0905   BRK
    = $0906   BRK
    = $0907   BRK
    = $0908   BRK
    = $0909   BRK
    = $090A   BRK
    = $090B   BRK
    = $090C   BRK
    = $090D   BRK
    = $090E   BRK
    = $090F   BRK
    = $0910   BRK
    = $0911   BRK
    = $0912   BRK

Some instructions.

    | byte foo
    | 
    | routine main
    |   trashes a, x, y, z, n, c, v, foo
    | {
    |     ld a, 0
    |     ld x, 0
    |     ld y, 0
    |     st a, foo
    |     st x, foo
    |     st y, foo
    |     st on, c
    |     st off, c
    |     add a, 1
    |     add a, foo
    |     sub a, 1
    |     sub a, foo
    |     inc foo
    |     inc x
    |     inc y
    |     dec foo
    |     dec x
    |     dec y
    |     and a, 255
    |     and a, foo
    |     or a, 255
    |     or a, foo
    |     xor a, 255
    |     xor a, foo
    |     cmp a, 1
    |     cmp a, foo
    |     cmp x, 1
    |     cmp x, foo
    |     cmp y, 1
    |     cmp y, foo
    |     shl a
    |     shr a
    | }
    = $080D   LDA #$00
    = $080F   LDX #$00
    = $0811   LDY #$00
    = $0813   STA $0853
    = $0816   STX $0853
    = $0819   STY $0853
    = $081C   SEC
    = $081D   CLC
    = $081E   ADC #$01
    = $0820   ADC $0853
    = $0823   SBC #$01
    = $0825   SBC $0853
    = $0828   INC $0853
    = $082B   INX
    = $082C   INY
    = $082D   DEC $0853
    = $0830   DEX
    = $0831   DEY
    = $0832   AND #$FF
    = $0834   AND $0853
    = $0837   ORA #$FF
    = $0839   ORA $0853
    = $083C   EOR #$FF
    = $083E   EOR $0853
    = $0841   CMP #$01
    = $0843   CMP $0853
    = $0846   CPX #$01
    = $0848   CPX $0853
    = $084B   CPY #$01
    = $084D   CPY $0853
    = $0850   ROL A
    = $0851   ROR A
    = $0852   RTS

Compiling `if`.

    | routine main
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     if z {
    |         ld y, 1
    |     } else {
    |         ld y, 2
    |     }
    | }
    = $080D   LDA #$00
    = $080F   BNE $0816
    = $0811   LDY #$01
    = $0813   JMP $0818
    = $0816   LDY #$02
    = $0818   RTS

Compiling `if not`.

    | routine main
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     if not z {
    |         ld y, 1
    |     } else {
    |         ld y, 2
    |     }
    | }
    = $080D   LDA #$00
    = $080F   BEQ $0816
    = $0811   LDY #$01
    = $0813   JMP $0818
    = $0816   LDY #$02
    = $0818   RTS

Compiling `if` without `else`.

    | routine main
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     if z {
    |         ld y, 1
    |     }
    | }
    = $080D   LDA #$00
    = $080F   BNE $0813
    = $0811   LDY #$01
    = $0813   RTS

Compiling `repeat`.

    | routine main
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         ld a, y
    |         inc y
    |         cmp y, 91
    |     } until z
    | }
    = $080D   LDY #$41
    = $080F   TYA
    = $0810   INY
    = $0811   CPY #$5B
    = $0813   BNE $080F
    = $0815   RTS

Compiling `repeat until not`.

    | routine main
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         ld a, y
    |         inc y
    |         cmp y, 91
    |     } until not z
    | }
    = $080D   LDY #$41
    = $080F   TYA
    = $0810   INY
    = $0811   CPY #$5B
    = $0813   BEQ $080F
    = $0815   RTS

Compiling `repeat forever`.

    | routine main
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         inc y
    |     } forever
    | }
    = $080D   LDY #$41
    = $080F   INY
    = $0810   JMP $080F
    = $0813   RTS

The body of `repeat forever` can be empty.

    | routine main
    | {
    |     repeat {
    |     } forever
    | }
    = $080D   JMP $080D
    = $0810   RTS

Indexed access.

    | byte one
    | byte table many
    | 
    | routine main
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st a, many + x
    |     ld a, many + x
    | }
    = $080D   LDX #$00
    = $080F   LDA #$00
    = $0811   STA $0819,X
    = $0814   LDA $0819,X
    = $0817   RTS

Byte tables take up 256 bytes in memory.

    | byte table tab1
    | byte table tab2
    | 
    | routine main
    |   inputs tab1
    |   outputs tab2
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, tab1 + x
    |     st a, tab2 + x
    | }
    = $080D   LDX #$00
    = $080F   LDA $0816,X
    = $0812   STA $0916,X
    = $0815   RTS

Byte storage locations take up only 1 byte in memory.

    | byte one
    | byte two
    | 
    | routine main
    |   outputs one, two
    |   trashes a, x, n, z
    | {
    |     ld a, 0
    |     st a, one
    |     st a, two
    | }
    = $080D   LDA #$00
    = $080F   STA $0816
    = $0812   STA $0817
    = $0815   RTS

Copy byte to byte.

    | byte bar
    | byte baz
    | 
    | routine main
    |   inputs baz
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy baz, bar
    | }
    = $080D   LDA $0815
    = $0810   STA $0814
    = $0813   RTS

Copy word to word.

    | word bar
    | word baz
    | 
    | routine main
    |   inputs baz
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy baz, bar
    | }
    = $080D   LDA $081C
    = $0810   STA $081A
    = $0813   LDA $081D
    = $0816   STA $081B
    = $0819   RTS

Copy literal word to word.

    | word bar
    | 
    | routine main
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy 2000, bar
    | }
    = $080D   LDA #$D0
    = $080F   STA $0818
    = $0812   LDA #$07
    = $0814   STA $0819
    = $0817   RTS

You can also copy a literal word to a word table.

    | word table many
    | 
    | routine main
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy 9999, many + x
    | }
    = $080D   LDX #$00
    = $080F   LDA #$0F
    = $0811   STA $081A,X
    = $0814   LDA #$27
    = $0816   STA $091A,X
    = $0819   RTS

Copy vector to vector.

    | vector bar
    | vector baz
    | 
    | routine main
    |   inputs baz
    |   outputs bar
    |   trashes a, n, z
    | {
    |   copy baz, bar
    | }
    = $080D   LDA $081C
    = $0810   STA $081A
    = $0813   LDA $081D
    = $0816   STA $081B
    = $0819   RTS

Copy routine to vector, inside an `interrupts off` block.

    | vector bar
    | 
    | routine foo
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | routine main
    |   inputs foo
    |   outputs bar
    |   trashes a, n, z
    | {
    |   with interrupts off {
    |     copy foo, bar
    |   }
    | }
    = $080D   SEI
    = $080E   LDA #$1A
    = $0810   STA $081C
    = $0813   LDA #$08
    = $0815   STA $081D
    = $0818   CLI
    = $0819   RTS
    = $081A   INX
    = $081B   RTS

Copy word to word table and back, with both `x` and `y` as indexes.

    | word one
    | word table many
    | 
    | routine main
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, y, n, z
    | {
    |     ld x, 0
    |     ld y, 0
    |     copy 777, one
    |     copy one, many + x
    |     copy one, many + y
    |     copy many + x, one
    |     copy many + y, one
    | }
    = $080D   LDX #$00
    = $080F   LDY #$00
    = $0811   LDA #$09
    = $0813   STA $084C
    = $0816   LDA #$03
    = $0818   STA $084D
    = $081B   LDA $084C
    = $081E   STA $084E,X
    = $0821   LDA $084D
    = $0824   STA $094E,X
    = $0827   LDA $084C
    = $082A   STA $084E,Y
    = $082D   LDA $084D
    = $0830   STA $094E,Y
    = $0833   LDA $084E,X
    = $0836   STA $084C
    = $0839   LDA $094E,X
    = $083C   STA $084D
    = $083F   LDA $084E,Y
    = $0842   STA $084C
    = $0845   LDA $094E,Y
    = $0848   STA $084D
    = $084B   RTS

Indirect call.

    | vector foo outputs x trashes z, n
    | 
    | routine bar outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | routine main inputs bar outputs x, foo trashes a, z, n {
    |     copy bar, foo
    |     call foo
    | }
    = $080D   LDA #$1B
    = $080F   STA $0821
    = $0812   LDA #$08
    = $0814   STA $0822
    = $0817   JSR $081E
    = $081A   RTS
    = $081B   LDX #$C8
    = $081D   RTS
    = $081E   JMP ($0821)

goto.

    | routine bar outputs x trashes z, n {
    |     ld x, 200
    | }
    | 
    | routine main outputs x trashes a, z, n {
    |     ld y, 200
    |     goto bar
    | }
    = $080D   LDY #$C8
    = $080F   JMP $0813
    = $0812   RTS
    = $0813   LDX #$C8
    = $0815   RTS

### word operations

Adding a constant word to a word memory location.

    | word score
    | routine main
    |   inputs score
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add score, 1999
    | }
    = $080D   CLC
    = $080E   LDA $081F
    = $0811   ADC #$CF
    = $0813   STA $081F
    = $0816   LDA $0820
    = $0819   ADC #$07
    = $081B   STA $0820
    = $081E   RTS

Adding a word memory location to another word memory location.

    | word score
    | word delta
    | routine main
    |   inputs score, delta
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st off, c
    |     add score, delta
    | }
    = $080D   CLC
    = $080E   LDA $0821
    = $0811   ADC $0823
    = $0814   STA $0821
    = $0817   LDA $0822
    = $081A   ADC $0824
    = $081D   STA $0822
    = $0820   RTS

Subtracting a constant word from a word memory location.

    | word score
    | routine main
    |   inputs score
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st on, c
    |     sub score, 1999
    | }
    = $080D   SEC
    = $080E   LDA $081F
    = $0811   SBC #$CF
    = $0813   STA $081F
    = $0816   LDA $0820
    = $0819   SBC #$07
    = $081B   STA $0820
    = $081E   RTS

Subtracting a word memory location from another word memory location.

    | word score
    | word delta
    | routine main
    |   inputs score, delta
    |   outputs score
    |   trashes a, c, z, v, n
    | {
    |     st on, c
    |     sub score, delta
    | }
    = $080D   SEC
    = $080E   LDA $0821
    = $0811   SBC $0823
    = $0814   STA $0821
    = $0817   LDA $0822
    = $081A   SBC $0824
    = $081D   STA $0822
    = $0820   RTS

### Buffers and Pointers

Load address into pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | 
    | routine main
    |   inputs buf
    |   outputs buf, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    | }
    = $080D   LDY #$00
    = $080F   LDA #$18
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   RTS

Write literal through a pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | 
    | routine main
    |   inputs buf
    |   outputs buf, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy 123, [ptr] + y
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1C
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA #$7B
    = $0819   STA ($FE),Y
    = $081B   RTS

Write stored value through a pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | byte foo
    | 
    | routine main
    |   inputs foo, buf
    |   outputs y, buf
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy foo, [ptr] + y
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1D
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA $101D
    = $081A   STA ($FE),Y
    = $081C   RTS

Read through a pointer.

    | buffer[2048] buf
    | pointer ptr @ 254
    | byte foo
    | 
    | routine main
    |   inputs buf
    |   outputs y, foo
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     copy ^buf, ptr
    |     copy [ptr] + y, foo
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1D
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA ($FE),Y
    = $0819   STA $101D
    = $081C   RTS

Add a word memory location, and a literal word, to a pointer, and then read through it.
Note that this is *not* range-checked.  (Yet.)

    | buffer[2048] buf
    | pointer ptr @ 254
    | byte foo
    | word delta
    | 
    | routine main
    |   inputs buf
    |   outputs y, foo, delta
    |   trashes a, z, n, ptr
    | {
    |     copy 619, delta
    |     ld y, 0
    |     copy ^buf, ptr
    |     add ptr, delta
    |     add ptr, word 1
    |     copy [ptr] + y, foo
    | }
    = $080D   LDA #$6B
    = $080F   STA $1042
    = $0812   LDA #$02
    = $0814   STA $1043
    = $0817   LDY #$00
    = $0819   LDA #$41
    = $081B   STA $FE
    = $081D   LDA #$08
    = $081F   STA $FF
    = $0821   LDA $FE
    = $0823   ADC $1042
    = $0826   STA $FE
    = $0828   LDA $FF
    = $082A   ADC $1043
    = $082D   STA $FF
    = $082F   LDA $FE
    = $0831   ADC #$01
    = $0833   STA $FE
    = $0835   LDA $FF
    = $0837   ADC #$00
    = $0839   STA $FF
    = $083B   LDA ($FE),Y
    = $083D   STA $1041
    = $0840   RTS

### Trash

Trash does nothing except indicate that we do not care about the value anymore.

    | routine main
    |   inputs a
    |   outputs x
    |   trashes a, z, n
    | {
    |     ld x, a
    |     ld a, 0
    |     trash a
    | }
    = $080D   TAX
    = $080E   LDA #$00
    = $0810   RTS
