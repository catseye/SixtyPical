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

Initialized byte table.  Bytes allocated, but beyond the string, are 0's.

    | byte table[8] message : "WHAT?"
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
    | byte table[256] many
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

    | byte table[256] tab1
    | byte table[256] tab2
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

    | word table[256] many
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
    | word table[256] many
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

    | vector outputs x
    |        trashes z, n
    |   foo
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

### Vector tables

Copying to and from a vector table.

    | vector
    |   outputs x
    |   trashes a, z, n
    |     one
    | vector
    |   outputs x
    |   trashes a, z, n
    |     table[256] many
    | 
    | routine bar outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | routine main
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, one
    |     copy one, many + x
    |     copy many + x, one
    |     call one
    | }
    = $080D   LDX #$00
    = $080F   LDA #$35
    = $0811   STA $083B
    = $0814   LDA #$08
    = $0816   STA $083C
    = $0819   LDA $083B
    = $081C   STA $083D,X
    = $081F   LDA $083C
    = $0822   STA $093D,X
    = $0825   LDA $083D,X
    = $0828   STA $083B
    = $082B   LDA $093D,X
    = $082E   STA $083C
    = $0831   JSR $0838
    = $0834   RTS
    = $0835   LDX #$C8
    = $0837   RTS
    = $0838   JMP ($083B)

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

Read through a pointer, into a byte storage location, or the `a` register.

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
    |     copy [ptr] + y, a
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1F
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA ($FE),Y
    = $0819   STA $101F
    = $081C   LDA ($FE),Y
    = $081E   RTS

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
