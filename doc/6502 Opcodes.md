6502 Opcodes
============

<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

As used or unused in SixtyPical.

### ld ###

    ld a, 123     → LDA #123
    ld a, lives   → LDA LIVES
    ld x, 123     → LDX #123
    ld x, lives   → LDX LIVES
    ld y, 123     → LDY #123
    ld y, lives   → LDY LIVES
    ld x, a       → TAX
    ld y, a       → TAY
    ld a, x       → TXA
    ld a, y       → TYA

### st ###

    st a, lives    → STA LIVES
    st x, lives    → STX LIVES
    st y, lives    → STY LIVES
    st on, c       → SEC
    st off, c      → CLC

### add dest, src ###

    add a, delta   → ADC DELTA
    add a, 1       → ADC #1

### inc ###

    inc x          → INX
    inc y          → INY
    inc lives      → INC LIVES

### sub ###

    sub a, delta   → SBC DELTA
    sub a, 1       → SBC #1

### dec ###

    dec x          → DEX
    dec y          → DEY
    dec lives      → DEC LIVES

### cmp ###

    cmp a, delta   → CMP DELTA
    cmp a, 1       → CMP #1
    cmp x, 1       → CPX #1
    cmp y, 1       → CPY #1

### and, or, xor ###

    and a, 8       → AND #8
    or a, 8        → ORA #8
    xor a, 8       → EOR #8

### shl, shr ###

    shl <dest-memory-location>
    shr <dest-memory-location>

    shl a          → ROL A
    shl lives      → ROL LIVES
    shr a          → ROR A
    shr lives      → ROR LIVES

### call ###

    call routine   → JSR ROUTINE

### if ###

    if z           → BEQ LABEL
    if not z       → BNE LABEL
    if n           → BMI LABEL
    if not n       → BPL LABEL
    if c           → BCS LABEL
    if not c       → BCC LABEL
    if v           → BVS LABEL
    if not v       → BVC LABEL

### 6502 instructions unsupported ###

    ASL   Shift Left One Bit (Memory or Accumulator)
    LSR   Shift Right One Bit (Memory or Accumulator)
    
    BIT   Test Bits in Memory with Accumulator
    BRK   Force Break
    
    CLD   Clear Decimal Mode
    CLI   Clear interrupt Disable Bit
    CLV   Clear Overflow Flag
    
    NOP   No Operation
    
    JMP   Jump to New Location       // but may be generated as part of `if`
    
    PHA   Push Accumulator on Stack
    PHP   Push Processor Status on Stack
    PLA   Pull Accumulator from Stack
    PLP   Pull Processor Status from Stack
    
    RTI   Return from Interrupt
    RTS   Return from Subroutine
    
    SED   Set Decimal Mode
    SEI   Set Interrupt Disable Status
    
    TSX   Transfer Stack Pointer to Index X
    TXS   Transfer Index X to Stack Pointer
