SixtyPical Compilation
======================

This is a test suite, written in [Falderal][] format, for compiling
SixtyPical to 6502 machine code.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Tests for functionality "Compile SixtyPical program"

Null program.

    | define main routine
    | {
    | }
    = $080D   RTS

`nop` program.

    | define main routine
    | {
    |     nop
    | }
    = $080D   NOP
    = $080E   RTS

Rudimentary program.

    | define main routine
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

    | define chrout routine
    |   inputs a
    |   trashes a
    |   @ 65490
    | 
    | define main routine
    |   inputs a
    |   trashes a, z, n
    | {
    |     ld a, 65
    |     call chrout
    |     ld a, 0
    | }
    = $080D   LDA #$41
    = $080F   JSR $FFD2
    = $0812   LDA #$00
    = $0814   RTS

Call defined routine.

    | define foo routine
    |   outputs a, x, y
    |   trashes z, n
    | {
    |   ld a, 0
    |   ld x, 0
    |   ld y, 0
    | }
    | 
    | define main routine
    |   trashes a, x, y, z, n
    | {
    |     call foo
    |     ld a, 1
    | }
    = $080D   JSR $0813
    = $0810   LDA #$01
    = $0812   RTS
    = $0813   LDA #$00
    = $0815   LDX #$00
    = $0817   LDY #$00
    = $0819   RTS

Tail call is optimized into a jump.

    | define foo routine
    |   outputs a, x, y
    |   trashes z, n
    | {
    |   ld a, 0
    |   ld x, 0
    |   ld y, 0
    | }
    | 
    | define main routine
    |   trashes a, x, y, z, n
    | {
    |     ld a, 1
    |     call foo
    | }
    = $080D   LDA #$01
    = $080F   JMP $0812
    = $0812   LDA #$00
    = $0814   LDX #$00
    = $0816   LDY #$00
    = $0818   RTS

Access a defined memory location.

    | byte foo
    | 
    | define main routine
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
    | define main routine
    |   trashes a, z, n, screen
    | {
    |   ld a, 100
    |   st a, screen
    | }
    = $080D   LDA #$64
    = $080F   STA $0400
    = $0812   RTS

Accesses to memory locations in zero-page with `ld` and `st`
and `and`, `or`, and `xor` use zero-page addressing.

    | byte zp @ $00
    | byte screen @ 100
    | 
    | define main routine
    |   inputs screen, zp
    |   outputs screen, zp
    |   trashes a, z, n
    | {
    |   ld a, screen
    |   st a, screen
    |   ld a, zp
    |   st a, zp
    |   and a, zp
    |   or a, zp
    |   xor a, zp
    | }
    = $080D   LDA $64
    = $080F   STA $64
    = $0811   LDA $00
    = $0813   STA $00
    = $0815   AND $00
    = $0817   ORA $00
    = $0819   EOR $00
    = $081B   RTS

Memory location with initial value.

    | byte lives : 3
    | 
    | define main routine
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
    | define main routine
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

Initialized byte table, initialized with ASCII string.  Bytes allocated, but beyond the string, are 0's.

    | byte table[8] message : "WHAT?"
    | 
    | define main routine
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

Initialized byte table, initialized with list of byte values.

    | byte table[8] message : 255, 0, 129, 128, 127
    | 
    | define main routine
    |   inputs message
    |   outputs x, a, z, n
    | {
    |   ld x, 0
    |   ld a, message + x
    | }
    = $080D   LDX #$00
    = $080F   LDA $0813,X
    = $0812   RTS
    = $0813   .byte $FF
    = $0814   BRK
    = $0815   STA ($80,X)
    = $0817   .byte $7F
    = $0818   BRK
    = $0819   BRK
    = $081A   BRK

Initialized word table, initialized with list of word values.

    | word table[4] message : 65535, 0, 127, 127
    | 
    | define main routine
    | {
    | }
    = $080D   RTS
    = $080E   .byte $FF
    = $080F   .byte $FF
    = $0810   BRK
    = $0811   BRK
    = $0812   .byte $7F
    = $0813   BRK
    = $0814   .byte $7F
    = $0815   BRK

Some instructions.

    | byte foo
    | 
    | define main routine
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
    |     shl foo
    |     shr foo
    | }
    = $080D   LDA #$00
    = $080F   LDX #$00
    = $0811   LDY #$00
    = $0813   STA $0859
    = $0816   STX $0859
    = $0819   STY $0859
    = $081C   SEC
    = $081D   CLC
    = $081E   ADC #$01
    = $0820   ADC $0859
    = $0823   SBC #$01
    = $0825   SBC $0859
    = $0828   INC $0859
    = $082B   INX
    = $082C   INY
    = $082D   DEC $0859
    = $0830   DEX
    = $0831   DEY
    = $0832   AND #$FF
    = $0834   AND $0859
    = $0837   ORA #$FF
    = $0839   ORA $0859
    = $083C   EOR #$FF
    = $083E   EOR $0859
    = $0841   CMP #$01
    = $0843   CMP $0859
    = $0846   CPX #$01
    = $0848   CPX $0859
    = $084B   CPY #$01
    = $084D   CPY $0859
    = $0850   ROL A
    = $0851   ROR A
    = $0852   ROL $0859
    = $0855   ROR $0859
    = $0858   RTS

Some instructions on tables. (1/3)

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z, v
    | {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     add a, many + x
    |     sub a, many + x
    |     cmp a, many + x
    | }
    = $080D   LDX #$00
    = $080F   LDA #$00
    = $0811   CLC
    = $0812   ADC $081C,X
    = $0815   SBC $081C,X
    = $0818   CMP $081C,X
    = $081B   RTS

Some instructions on tables. (2/3)

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     and a, many + x
    |     or a, many + x
    |     xor a, many + x
    | }
    = $080D   LDX #$00
    = $080F   LDA #$00
    = $0811   AND $081B,X
    = $0814   ORA $081B,X
    = $0817   EOR $081B,X
    = $081A   RTS

Some instructions on tables. (3/3)

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z
    | {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     shl many + x
    |     shr many + x
    |     inc many + x
    |     dec many + x
    | }
    = $080D   LDX #$00
    = $080F   LDA #$00
    = $0811   CLC
    = $0812   ROL $081F,X
    = $0815   ROR $081F,X
    = $0818   INC $081F,X
    = $081B   DEC $081F,X
    = $081E   RTS

Using a constant offset, you can read and write entries in
the table beyond the 256th.

    | byte one
    | byte table[1024] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     ld a, many + x
    |     st a, many + x
    |     ld a, many + 999 + x
    |     st a, many + 1000 + x
    | }
    = $080D   LDX #$00
    = $080F   LDA $081D,X
    = $0812   STA $081D,X
    = $0815   LDA $0C04,X
    = $0818   STA $0C05,X
    = $081B   RTS

Instructions on tables, with constant offsets.

    | byte table[256] many
    | 
    | define main routine
    |   inputs many
    |   outputs many
    |   trashes a, x, c, n, z, v
    | {
    |     ld x, 0
    |     ld a, 0
    |     st off, c
    |     add a, many + 1 + x
    |     sub a, many + 2 + x
    |     cmp a, many + 3 + x
    |     and a, many + 4 + x
    |     or a, many + 5 + x
    |     xor a, many + 6 + x
    |     shl many + 7 + x
    |     shr many + 8 + x
    |     inc many + 9 + x
    |     dec many + 10 + x
    | }
    = $080D   LDX #$00
    = $080F   LDA #$00
    = $0811   CLC
    = $0812   ADC $0832,X
    = $0815   SBC $0833,X
    = $0818   CMP $0834,X
    = $081B   AND $0835,X
    = $081E   ORA $0836,X
    = $0821   EOR $0837,X
    = $0824   ROL $0838,X
    = $0827   ROR $0839,X
    = $082A   INC $083A,X
    = $082D   DEC $083B,X
    = $0830   RTS

Compiling 16-bit `cmp`.

    | word za @ 60001
    | word zb : 3003
    | 
    | define main routine
    |   inputs za, zb
    |   trashes a, z, c, n
    | {
    |     cmp za, zb
    |     cmp za, 4000
    | }
    = $080D   LDA $EA61
    = $0810   CMP $0828
    = $0813   BNE $081B
    = $0815   LDA $EA62
    = $0818   CMP $0829
    = $081B   LDA $EA61
    = $081E   CMP #$A0
    = $0820   BNE $0827
    = $0822   LDA $EA62
    = $0825   CMP #$0F
    = $0827   RTS
    = $0828   .byte $BB
    = $0829   .byte $0B

Compiling `if`.

    | define main routine
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

    | define main routine
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

    | define main routine
    |   trashes a, x, y, z, n, c, v
    | {
    |     ld a, 0
    |     ld y, 0
    |     if z {
    |         ld y, 1
    |     }
    | }
    = $080D   LDA #$00
    = $080F   LDY #$00
    = $0811   BNE $0815
    = $0813   LDY #$01
    = $0815   RTS

Compiling `repeat ... until z`.

    | define main routine
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

Compiling `repeat ... until not z`.

    | define main routine
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

Compiling `repeat ... until n`.

    | define main routine
    |   trashes a, y, z, n, c
    | {
    |     ld y, 65
    |     repeat {
    |         ld a, y
    |         dec y
    |     } until n
    | }
    = $080D   LDY #$41
    = $080F   TYA
    = $0810   DEY
    = $0811   BPL $080F
    = $0813   RTS

Compiling `repeat ... until not n`.

    | define main routine
    |   trashes a, y, z, n, c
    | {
    |     ld y, 199
    |     repeat {
    |         ld a, y
    |         inc y
    |     } until not n
    | }
    = $080D   LDY #$C7
    = $080F   TYA
    = $0810   INY
    = $0811   BMI $080F
    = $0813   RTS

Compiling `repeat forever`.

    | define main routine
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

The body of `repeat forever` can be empty.

    | define main routine
    | {
    |     repeat {
    |     } forever
    | }
    = $080D   JMP $080D

Compiling `for ... up to`.

    | byte table[256] tab
    | 
    | define main routine
    |   inputs tab
    |   trashes a, x, c, z, v, n
    | {
    |     ld x, 0
    |     for x up to 15 {
    |         ld a, tab + x
    |     }
    | }
    = $080D   LDX #$00
    = $080F   LDA $0818,X
    = $0812   INX
    = $0813   CPX #$10
    = $0815   BNE $080F
    = $0817   RTS

Compiling `for ... down to`.

    | byte table[256] tab
    | 
    | define main routine
    |   inputs tab
    |   trashes a, x, c, z, v, n
    | {
    |     ld x, 15
    |     for x down to 0 {
    |         ld a, tab + x
    |     }
    | }
    = $080D   LDX #$0F
    = $080F   LDA $0818,X
    = $0812   DEX
    = $0813   CPX #$FF
    = $0815   BNE $080F
    = $0817   RTS

Compiling `save`.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes z, n
    | {
    |     save a {
    |         save x {
    |             ld a, 0
    |             ld x, 1
    |         }
    |     }
    | }
    = $080D   PHA
    = $080E   TXA
    = $080F   PHA
    = $0810   LDA #$00
    = $0812   LDX #$01
    = $0814   PLA
    = $0815   TAX
    = $0816   PLA
    = $0817   RTS

Compiling `save` with shortcut syntax.

    | define main routine
    |   inputs a
    |   outputs a
    |   trashes z, n
    | {
    |     save a, x {
    |         ld a, 0
    |         ld x, 1
    |     }
    | }
    = $080D   PHA
    = $080E   TXA
    = $080F   PHA
    = $0810   LDA #$00
    = $0812   LDX #$01
    = $0814   PLA
    = $0815   TAX
    = $0816   PLA
    = $0817   RTS

Compiling `save` on a user-defined location.

    | byte foo
    | define main routine
    |   trashes a, z, n
    | {
    |     save foo {
    |         ld a, 0
    |         st a, foo
    |     }
    | }
    = $080D   LDA $081B
    = $0810   PHA
    = $0811   LDA #$00
    = $0813   STA $081B
    = $0816   PLA
    = $0817   STA $081B
    = $081A   RTS

Indexed access.

    | byte one
    | byte table[256] many
    | 
    | define main routine
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

Byte tables take up, at most, 256 bytes in memory.

    | byte table[256] tab1
    | byte table[256] tab2
    | 
    | define main routine
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
    | define main routine
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
    | define main routine
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
    | define main routine
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
    | define main routine
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
    | define main routine
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

    | vector routine bar
    | vector routine baz
    | 
    | define main routine
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

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     bar
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    | 
    | define main routine
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

Copy routine (by forward reference) to vector.

    | vector routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |     bar
    | 
    | define main routine
    |   outputs bar
    |   trashes a, n, z
    | {
    |     copy foo, bar
    | }
    | 
    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    | {
    |     inc x
    | }
    = $080D   LDA #$18
    = $080F   STA $081A
    = $0812   LDA #$08
    = $0814   STA $081B
    = $0817   RTS
    = $0818   INX
    = $0819   RTS

Copy byte to byte table and back, with both `x` and `y` as indexes,
plus constant offsets.

    | byte one
    | byte table[256] many
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, y, n, z
    | {
    |     ld x, 0
    |     ld y, 0
    |     ld a, 77
    |     st a, many + x
    |     st a, many + y
    |     st a, many + 1 + x
    |     st a, many + 1 + y
    |     ld a, many + x
    |     ld a, many + y
    |     ld a, many + 8 + x
    |     ld a, many + 8 + y
    | }
    = $080D   LDX #$00
    = $080F   LDY #$00
    = $0811   LDA #$4D
    = $0813   STA $082D,X
    = $0816   STA $082D,Y
    = $0819   STA $082E,X
    = $081C   STA $082E,Y
    = $081F   LDA $082D,X
    = $0822   LDA $082D,Y
    = $0825   LDA $0835,X
    = $0828   LDA $0835,Y
    = $082B   RTS

Copy word to word table and back, with both `x` and `y` as indexes.

    | word one
    | word table[256] many
    | 
    | define main routine
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

Copy word to word table and back, with constant offsets.

    | word one
    | word table[256] many
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, y, n, z
    | {
    |     ld x, 0
    |     ld y, 0
    |     copy 777, one
    |     copy one, many + 1 + x
    |     copy one, many + 2 + y
    |     copy many + 3 + x, one
    |     copy many + 4 + y, one
    | }
    = $080D   LDX #$00
    = $080F   LDY #$00
    = $0811   LDA #$09
    = $0813   STA $084C
    = $0816   LDA #$03
    = $0818   STA $084D
    = $081B   LDA $084C
    = $081E   STA $084F,X
    = $0821   LDA $084D
    = $0824   STA $094F,X
    = $0827   LDA $084C
    = $082A   STA $0850,Y
    = $082D   LDA $084D
    = $0830   STA $0950,Y
    = $0833   LDA $0851,X
    = $0836   STA $084C
    = $0839   LDA $0951,X
    = $083C   STA $084D
    = $083F   LDA $0852,Y
    = $0842   STA $084C
    = $0845   LDA $0952,Y
    = $0848   STA $084D
    = $084B   RTS

Indirect call.  TODO: we don't need the final RTS here, omit it.

    | vector routine
    |   outputs x
    |   trashes z, n
    |     foo
    | 
    | define bar routine
    |   outputs x
    |   trashes z, n
    | {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   outputs x, foo
    |   trashes a, z, n
    | {
    |     copy bar, foo
    |     call foo
    | }
    = $080D   LDA #$1A
    = $080F   STA $0821
    = $0812   LDA #$08
    = $0814   STA $0822
    = $0817   JMP $081D
    = $081A   LDX #$C8
    = $081C   RTS
    = $081D   JMP ($0821)
    = $0820   RTS

Compiling `goto`.  Note that no `RTS` is emitted after the `JMP`.

    | define bar routine
    |   inputs y
    |   outputs x, y
    |   trashes z, n
    | {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   outputs x, y
    |   trashes a, z, n
    | {
    |     ld y, 200
    |     goto bar
    | }
    = $080D   LDY #$C8
    = $080F   JMP $0812
    = $0812   LDX #$C8
    = $0814   RTS

### Vector tables

Copying to and from a vector table.

    | vector routine
    |   outputs x
    |   trashes a, z, n
    |     one
    | vector routine
    |   outputs x
    |   trashes a, z, n
    |     table[256] many
    | 
    | define bar routine outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, one
    |     copy bar, many + x
    |     copy one, many + x
    |     copy many + x, one
    |     call one
    | }
    = $080D   LDX #$00
    = $080F   LDA #$3E
    = $0811   STA $0845
    = $0814   LDA #$08
    = $0816   STA $0846
    = $0819   LDA #$3E
    = $081B   STA $0847,X
    = $081E   LDA #$08
    = $0820   STA $0947,X
    = $0823   LDA $0845
    = $0826   STA $0847,X
    = $0829   LDA $0846
    = $082C   STA $0947,X
    = $082F   LDA $0847,X
    = $0832   STA $0845
    = $0835   LDA $0947,X
    = $0838   STA $0846
    = $083B   JMP $0841
    = $083E   LDX #$C8
    = $0840   RTS
    = $0841   JMP ($0845)
    = $0844   RTS

Copying to and from a vector table, with constant offsets.

    | vector routine
    |   outputs x
    |   trashes a, z, n
    |     one
    | vector routine
    |   outputs x
    |   trashes a, z, n
    |     table[256] many
    | 
    | define bar routine outputs x trashes a, z, n {
    |     ld x, 200
    | }
    | 
    | define main routine
    |   inputs one, many
    |   outputs one, many
    |   trashes a, x, n, z
    | {
    |     ld x, 0
    |     copy bar, one
    |     copy bar, many + 1 + x
    |     copy one, many + 2 + x
    |     copy many + 3 + x, one
    |     call one
    | }
    = $080D   LDX #$00
    = $080F   LDA #$3E
    = $0811   STA $0845
    = $0814   LDA #$08
    = $0816   STA $0846
    = $0819   LDA #$3E
    = $081B   STA $0848,X
    = $081E   LDA #$08
    = $0820   STA $0948,X
    = $0823   LDA $0845
    = $0826   STA $0849,X
    = $0829   LDA $0846
    = $082C   STA $0949,X
    = $082F   LDA $084A,X
    = $0832   STA $0845
    = $0835   LDA $094A,X
    = $0838   STA $0846
    = $083B   JMP $0841
    = $083E   LDX #$C8
    = $0840   RTS
    = $0841   JMP ($0845)
    = $0844   RTS

### add, sub

Various modes of `add`.

    | byte lives
    | byte extra
    | word score
    | word bonus
    | define main routine
    |   inputs lives, score, extra, bonus
    |   outputs lives, score
    |   trashes a, x, y, c, z, v, n
    | {
    |     ld a, 0
    |     ld x, 0
    |     ld y, 0
    |     st off, c
    |     add a, 7
    |     add a, lives
    |     add lives, 2
    |     add lives, extra
    |     add score, 1999
    |     add score, bonus
    | }
    = $080D   LDA #$00
    = $080F   LDX #$00
    = $0811   LDY #$00
    = $0813   CLC
    = $0814   ADC #$07
    = $0816   ADC $084D
    = $0819   LDA $084D
    = $081C   ADC #$02
    = $081E   STA $084D
    = $0821   LDA $084D
    = $0824   ADC $084E
    = $0827   STA $084D
    = $082A   LDA $084F
    = $082D   ADC #$CF
    = $082F   STA $084F
    = $0832   LDA $0850
    = $0835   ADC #$07
    = $0837   STA $0850
    = $083A   LDA $084F
    = $083D   ADC $0851
    = $0840   STA $084F
    = $0843   LDA $0850
    = $0846   ADC $0852
    = $0849   STA $0850
    = $084C   RTS

Various modes of `sub`.

    | byte lives
    | byte extra
    | word score
    | word bonus
    | define main routine
    |   inputs lives, score, extra, bonus
    |   outputs lives, score
    |   trashes a, x, y, c, z, v, n
    | {
    |     ld a, 0
    |     ld x, 0
    |     ld y, 0
    |     st on, c
    |     sub a, 7
    |     sub a, lives
    |     sub lives, 2
    |     sub lives, extra
    |     sub score, 1999
    |     sub score, bonus
    | }
    = $080D   LDA #$00
    = $080F   LDX #$00
    = $0811   LDY #$00
    = $0813   SEC
    = $0814   SBC #$07
    = $0816   SBC $084D
    = $0819   LDA $084D
    = $081C   SBC #$02
    = $081E   STA $084D
    = $0821   LDA $084D
    = $0824   SBC $084E
    = $0827   STA $084D
    = $082A   LDA $084F
    = $082D   SBC #$CF
    = $082F   STA $084F
    = $0832   LDA $0850
    = $0835   SBC #$07
    = $0837   STA $0850
    = $083A   LDA $084F
    = $083D   SBC $0851
    = $0840   STA $084F
    = $0843   LDA $0850
    = $0846   SBC $0852
    = $0849   STA $0850
    = $084C   RTS

### word operations

Adding a constant word to a word memory location.

    | word score
    | define main routine
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
    | define main routine
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
    | define main routine
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
    | define main routine
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

### Tables and Pointers

Associate pointer with table.  Does nothing by itself.

    | byte table[256] tab
    | pointer ptr @ 254
    | 
    | define main routine
    |   inputs tab
    |   outputs tab, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |     }
    | }
    = $080D   LDY #$00
    = $080F   RTS

Reset pointer to table.

    | byte table[256] tab
    | pointer ptr @ 254
    | 
    | define main routine
    |   inputs tab
    |   outputs tab, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |     }
    | }
    = $080D   LDY #$00
    = $080F   LDA #$18
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   RTS

Write literal through a pointer.

    | byte table[256] tab
    | pointer ptr @ 254
    | 
    | define main routine
    |   inputs tab
    |   outputs tab, y
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy 123, [ptr] + y
    |     }
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

    | byte table[256] tab
    | pointer ptr @ 254
    | byte foo
    | 
    | define main routine
    |   inputs foo, tab
    |   outputs y, tab
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy foo, [ptr] + y
    |     }
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1D
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA $091D
    = $081A   STA ($FE),Y
    = $081C   RTS

Read through a pointer, into a byte storage location, or the `a` register.

    | byte table[256] tab
    | pointer ptr @ 254
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs y, foo
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         copy [ptr] + y, foo
    |         ld a, [ptr] + y
    |     }
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1F
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA ($FE),Y
    = $0819   STA $091F
    = $081C   LDA ($FE),Y
    = $081E   RTS

Multiple `reset`s may occur inside the same block.

    | byte table[256] tab @ 1024
    | pointer ptr @ 254
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs y, foo
    |   trashes a, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 16
    |         copy [ptr] + y, foo
    |         reset ptr 18
    |         ld a, [ptr] + y
    |     }
    | }
    = $080D   LDY #$00
    = $080F   LDA #$10
    = $0811   STA $FE
    = $0813   LDA #$04
    = $0815   STA $FF
    = $0817   LDA ($FE),Y
    = $0819   STA $0827
    = $081C   LDA #$12
    = $081E   STA $FE
    = $0820   LDA #$04
    = $0822   STA $FF
    = $0824   LDA ($FE),Y
    = $0826   RTS

Read and write through two pointers.

    | byte table[256] tab
    | pointer ptra @ 252
    | pointer ptrb @ 254
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, z, n, ptra, ptrb
    | {
    |     ld y, 0
    |     point ptra into tab {
    |         reset ptra 0
    |         point ptrb into tab {
    |             reset ptrb 0
    |             copy [ptra] + y, [ptrb] + y
    |         }
    |     }
    | }
    = $080D   LDY #$00
    = $080F   LDA #$24
    = $0811   STA $FC
    = $0813   LDA #$08
    = $0815   STA $FD
    = $0817   LDA #$24
    = $0819   STA $FE
    = $081B   LDA #$08
    = $081D   STA $FF
    = $081F   LDA ($FC),Y
    = $0821   STA ($FE),Y
    = $0823   RTS

Write the `a` register through a pointer.

    | byte table[256] tab
    | pointer ptr @ 254
    | byte foo
    | 
    | define main routine
    |   inputs tab
    |   outputs tab
    |   trashes a, y, z, n, ptr
    | {
    |     ld y, 0
    |     point ptr into tab {
    |         reset ptr 0
    |         ld a, 255
    |         st a, [ptr] + y
    |     }
    | }
    = $080D   LDY #$00
    = $080F   LDA #$1C
    = $0811   STA $FE
    = $0813   LDA #$08
    = $0815   STA $FF
    = $0817   LDA #$FF
    = $0819   STA ($FE),Y
    = $081B   RTS

Add a word memory location, and a literal word, to a pointer, and then read through it.
Note that this is *not* range-checked.  (Yet.)

    | byte table[256] tab
    | pointer ptr @ 254
    | byte foo
    | word delta
    | 
    | define main routine
    |   inputs tab
    |   outputs y, foo, delta
    |   trashes a, c, v, z, n, ptr
    | {
    |     copy 619, delta
    |     ld y, 0
    |     st off, c
    |     point ptr into tab {
    |         reset ptr 0
    |         add ptr, delta
    |         add ptr, word 1
    |         copy [ptr] + y, foo
    |     }
    | }
    = $080D   LDA #$6B
    = $080F   STA $0943
    = $0812   LDA #$02
    = $0814   STA $0944
    = $0817   LDY #$00
    = $0819   CLC
    = $081A   LDA #$42
    = $081C   STA $FE
    = $081E   LDA #$08
    = $0820   STA $FF
    = $0822   LDA $FE
    = $0824   ADC $0943
    = $0827   STA $FE
    = $0829   LDA $FF
    = $082B   ADC $0944
    = $082E   STA $FF
    = $0830   LDA $FE
    = $0832   ADC #$01
    = $0834   STA $FE
    = $0836   LDA $FF
    = $0838   ADC #$00
    = $083A   STA $FF
    = $083C   LDA ($FE),Y
    = $083E   STA $0942
    = $0841   RTS

### Trash

Trash does nothing except indicate that we do not care about the value anymore.

    | define main routine
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

### locals ###

Memory locations defined local static to a routine are allocated
just the same as initialized global storage locations are.

    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   static byte t : 255
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    | 
    | define main routine
    |   trashes a, x, z, n
    |   static byte t : 7
    | {
    |   ld x, t
    |   call foo
    | }
    = $080D   LDX $081E
    = $0810   JMP $0813
    = $0813   STX $081D
    = $0816   INC $081D
    = $0819   LDX $081D
    = $081C   RTS
    = $081D   .byte $FF
    = $081E   .byte $07

Memory locations defined local dynamic to a routine are allocated
just the same as uninitialized global storage locations are.

    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   local byte t
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    | 
    | define main routine
    |   trashes a, x, z, n
    |   local byte t
    | {
    |   ld x, 0
    |   st x, t
    |   call foo
    | }
    = $080D   LDX #$00
    = $080F   STX $0820
    = $0812   JMP $0815
    = $0815   STX $081F
    = $0818   INC $081F
    = $081B   LDX $081F
    = $081E   RTS

Memory locations defined local dynamic to a routine are allocated
just the same as uninitialized global storage locations are, even
when declared with a fixed address.

    | define foo routine
    |   inputs x
    |   outputs x
    |   trashes z, n
    |   local byte t @ 1024
    | {
    |   st x, t
    |   inc t
    |   ld x, t
    | }
    | 
    | define main routine
    |   trashes a, x, z, n
    |   local byte t @ 1025
    | {
    |   ld x, 0
    |   st x, t
    |   call foo
    | }
    = $080D   LDX #$00
    = $080F   STX $0401
    = $0812   JMP $0815
    = $0815   STX $0400
    = $0818   INC $0400
    = $081B   LDX $0400
    = $081E   RTS
