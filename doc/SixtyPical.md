SixtyPical
==========

This document describes the SixtyPical programming language version 0.2,
both its execution aspect and its static analysis aspect (even though
these are, technically speaking, separate concepts.)

This document is nominally normative, but the tests in the `tests` directory
are even more normative.

Types
-----

There are two TYPES in SixtyPical:

*   bit (2 possible values)
*   byte (256 possible values)

Memory locations
----------------

A primary concept in SixtyPical is the MEMORY LOCATION.  At any given point
in time during execution, each memory location is either UNINITIALIZED or
INITIALIZED.  At any given point in the program text, too, each memory
location is either uninitialized or initialized.  Where-ever it is one or
the other during execution, it is the same in the corresponding place in
the program text; thus, it is a static property.

There are four general kinds of memory location.  The first three are
pre-defined and built-in.

### Registers ###

Each of these hold a byte.  They are initially uninitialized.

    a
    x
    y

### Flags ###

Each of these hold a bit.  They are initially uninitialized.

    c (carry)
    z (zero)
    v (overflow)
    n (negative)

### Constants ###

It may be strange to think of constants as memory locations, but keep in mind
that a memory location in SixtyPical need not map to a memory location in the
underlying hardware.  All constants are read-only.  Each is initially
initialized with the value that corresponds with its name.

They come in bit and byte types.  There are two bit constants,

    off
    on

and two-hundred and fifty-six byte constants,

    0
    1
    ...
    255

### User-defined ###

There may be any number of user-defined memory locations.  They are defined
by giving the type, which must be `byte`, and the name.

    byte pos

Routines
--------

Every routine must list all the memory locations it READS from, i.e. its
INPUTS, and all the memory locations it WRITES to, whether they are OUTPUTS
or merely TRASHED.  Every memory location that is not written to by the
routine (or any routines that the routine calls) is PRESERVED by the routine.

    routine foo
      inputs a, score
      outputs x
      trashes y {
        ...
    }

Routines may call only routines previously defined in the program source.
Thus, recursive routines are not allowed.

For a SixtyPical program to be run, there must be one routine called `main`.
This routine is executed when the program is run.

The memory locations given given as inputs are considered to be initialized
at the beginning of the routine.  Various instructions cause memory locations
to be initialized after they are executed.  Calling a routine which trashes
some memory locations causes those memory locations to be uninitialized after
that routine is called.  At the end of a routine, all memory locations listed
as outputs must be initialised.

Instructions
------------

### ld ###

    ld <dest-memory-location>, <src-memory-location>

Reads from src and writes to dest.

*   It is illegal if dest is not a register.
*   It is illegal if dest does not occur in the WRITES lists of the current
    routine.
*   It is illegal if src is not of same type as dest (i.e., is not a byte.)
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized.  The flags `z` and `n` may be
changed by this instruction; they must be named in the WRITES lists, and they
are considered initialized after it has executed.

Some combinations, such as `ld x, y`, are illegal because they do not map to
underlying opcodes.

Notes:

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

    st <src-memory-location>, <dest-memory-location>

Reads from src and writes to dest.

*   It is illegal if dest is a register or if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists of the current
    routine.
*   It is illegal if src is not of same type as dest.
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized.  No flags are
changed by this instruction (unless of course dest is a flag.)

Notes:

    st a, lives    → STA LIVES
    st x, lives    → STX LIVES
    st y, lives    → STY LIVES
    st on, c       → SEC
    st off, c      → CLC

### add dest, src ###

    add <dest-memory-location>, <src-memory-location>

Adds the contents of src to dest and stores the result in dest.

*   It is illegal if src OR dest OR c is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n, z, c, and v flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

dest and src continue to be initialized afterwards.

Notes:

    add a, delta   → ADC DELTA
    add a, 1       → ADC #1

### inc ###

    inc <dest-memory-location>

Increments the value in dest.  Does not honour carry.

*   It is illegal if dest is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n and z flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

Notes:

    inc x          → INX
    inc y          → INY
    inc lives      → INC LIVES

### sub ###

    sub <dest-memory-location>, <src-memory-location>

Subtracts the contents of src from dest and stores the result in dest.

*   It is illegal if src OR dest OR c is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n, z, c, and v flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

dest and src continue to be initialized afterwards.

Notes:

    sub a, delta   → SBC DELTA
    sub a, 1       → SBC #1

### dec ###

    inc <dest-memory-location>

Decrements the value in dest.  Does not honour carry.

*   It is illegal if dest is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n and z flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

Notes:

    dec x          → DEX
    dec y          → DEY
    dec lives      → DEC LIVES

### cmp ###

    cmp <dest-memory-location>, <src-memory-location>

Subtracts the contents of src from dest, but does not store the result.

*   It is illegal if src OR dest is uninitialized.

Affects n, z, and c flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

Notes:

    cmp a, delta   → CMP DELTA
    cmp a, 1       → CMP #1
    cmp x, 1       → CPX #1
    cmp y, 1       → CPY #1

- - - -

### and ###

    and <dest-memory-location>, <src-memory-location>

"AND"s the contents of src with dest and stores the result in dest.

The constraints are the same as for `cmp`, except that the `c` flag
is not affected.  i.e. only `n` and `z` flags are affected.

Notes:

    and a, 8       → AND #8

### or ###

    or <dest-memory-location>, <src-memory-location>

"OR"s the contents of src with dest and stores the result in dest.

The constraints and effects are exactly the same as for `and`.

Notes:
    
    or a, 8        → ORA #8

### xor ###

    xor <dest-memory-location>, <src-memory-location>

"XOR"s the contents of src with dest and stores the result in dest.

The constraints and effects are exactly the same as for `and`.

Notes:
    
    xor a, 8       → EOR #8

### shl ###

    shl <dest-memory-location>

Shifts the dest left one bit position.  The rightmost position becomes `c`,
and `c` becomes the bit that was shifted off the left.

*   It is illegal if dest is a register besides `a`.
*   It is illegal if dest is read-only.
*   It is illegal if dest OR c is uninitialized.
*   It is illegal if dest does not occur in the WRITES AND READS lists
    of the current routine.

Notes:

    shl a          → ROL A
    shl lives      → ROL LIVES

### shr ###

    shr <dest-memory-location>

Shifts the dest right one bit position.  The leftmost position becomes `c`,
and `c` becomes the bit that was shifted off the right.

Constraints are exactly the same as for `shl`.

Notes:

    shr a          → ROR A
    shr lives      → ROR LIVES

### call ###

    call <routine-name>

Just before the call,

*   It is illegal if any of the memory locations in the routine's READS list is
    uninitialized.

Just after the call,

*   All memory locations listed as TRASHED in the routine's WRITES list are
    considered uninitialized.

Notes:

    call routine    → JSR ROUTINE

### if ###

    if (bit) {
        true-branch
    } else {
        false-branch
    }

_bit_ is usually one of the flags, z or c.

Notes:

        BEQ   Branch on Result Zero
        BMI   Branch on Result Minus
        BNE   Branch on Result not Zero
        BPL   Branch on Result Plus
        BCC   Branch on Carry Clear
        BCS   Branch on Carry Set
        BVC   Branch on Overflow Clear
        BVS   Branch on Overflow Set


- - - -

Grammar
-------

    Program ::= {Defn} {Routine}.
    Defn    ::= "byte" NewIdent.
    Routine ::= "routine" NewIdent
                ["inputs" LocExprs] ["outputs" LocExprs] ["trashes" LocExprs]
                Block.
    LocExprs::= LocExpr {"," LocExpr}.
    LocExpr ::= Register | Flag | Const | DefnIdent.
    Register::= "a" | "x" | "y".
    Flag    ::= "c" | "z" | "n" | "v".
    Const   ::= "0" ... "255".
    Block   ::= "{" {Instr} "}".
    Instr   ::= "ld" LocExpr "," LocExpr
              | "st" LocExpr "," LocExpr
              | "add" LocExpr "," LocExpr
              | "sub" LocExpr "," LocExpr
              | "cmp" LocExpr "," LocExpr
              | "and" LocExpr "," LocExpr
              | "or" LocExpr "," LocExpr
              | "xor" LocExpr "," LocExpr
              | "shl" LocExpr
              | "shr" LocExpr
              | "inc" LocExpr
              | "dec" LocExpr
              | "call" RoutineIdent
              | "if" LocExpr Block ["else" Block].


### 6502 instructions unsupported ###

        ASL   Shift Left One Bit (Memory or Accumulator)
        LSR   Shift Right One Bit (Memory or Accumulator)

        BIT   Test Bits in Memory with Accumulator
        BRK   Force Break

        CLD   Clear Decimal Mode
        CLI   Clear interrupt Disable Bit
        CLV   Clear Overflow Flag

        NOP   No Operation

        JMP   Jump to New Location   // but may be generated as part of `if`

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
