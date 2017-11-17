SixtyPical
==========

This document describes the SixtyPical programming language version 0.7,
both its execution aspect and its static analysis aspect (even though
these are, technically speaking, separate concepts.)

This document is nominally normative, but the tests in the `tests` directory
are even more normative.

Refer to the bottom of this document for an EBNF grammar of the syntax of
the language.

Types
-----

There are five TYPES in SixtyPical:

*   bit (2 possible values)
*   byte (256 possible values)
*   byte table (256 entries, each holding a byte)
*   routine (code stored somewhere in memory, read-only)
*   vector (address of a routine)

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
by giving the type, which must be `byte`, `byte table`, or `vector`, and the
name.

    byte pos

An address in memory may be given explicitly on a user-defined memory location.

    byte table screen @ 1024

Or, a user-defined memory location may be given an initial value.  But in this
case, an explicit address in memory cannot be given.

    byte pos : 0

A user-defined vector memory location is decorated with READS and WRITES lists
like a routine (see below), and it may only hold addresses of routines which
are compatible.  (Meaning, the routine's inputs (resp. outputs, trashes)
must be a subset of the vector's inputs (resp. outputs, trashes.))

    vector actor_logic
      inputs a, score
      outputs x
      trashes y
      @ $c000

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
Thus, directly recursive routines are not allowed.  (However, routines may
also call routines via vectors, which are dynamically assigned.  In this
case, there is, for the time being, no check for recursive calls.)

For a SixtyPical program to be run, there must be one routine called `main`.
This routine is executed when the program is run.

The memory locations given given as inputs are considered to be initialized
at the beginning of the routine.  Various instructions cause memory locations
to be initialized after they are executed.  Calling a routine which trashes
some memory locations causes those memory locations to be uninitialized after
that routine is called.  At the end of a routine, all memory locations listed
as outputs must be initialised.

A routine can also be declared as "external", in which case its body need
not be defined but an absolute address must be given for where the routine
is located in memory.

    routine chrout
      inputs a
      trashes a
      @ 65490

Instructions
------------

### ld ###

    ld <dest-memory-location>, <src-memory-location> [+ <index-memory-location>]

Reads from src and writes to dest.

*   It is illegal if dest is not a register.
*   It is illegal if dest does not occur in the WRITES lists of the current
    routine.
*   It is illegal if src is not of same type as dest (i.e., is not a byte.)
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized.  The flags `z` and `n` may be
changed by this instruction; they must be named in the WRITES lists, and they
are considered initialized after it has executed.

If and only if src is a byte table, the index-memory-location must be given.

Some combinations, such as `ld x, y`, are illegal because they do not map to
underlying opcodes.

### st ###

    st <src-memory-location>, <dest-memory-location> [+ <index-memory-location>]

Reads from src and writes to dest.

*   It is illegal if dest is a register or if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists of the current
    routine.
*   It is illegal if src is not of same type as dest.
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized.  No flags are
changed by this instruction (unless of course dest is a flag.)

If and only if dest is a byte table, the index-memory-location must be given.

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

### inc ###

    inc <dest-memory-location>

Increments the value in dest.  Does not honour carry.

*   It is illegal if dest is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n and z flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

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

### dec ###

    dec <dest-memory-location>

Decrements the value in dest.  Does not honour carry.

*   It is illegal if dest is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n and z flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

### cmp ###

    cmp <dest-memory-location>, <src-memory-location>

Subtracts the contents of src from dest (without considering carry) but
does not store the result anywhere, only sets the resulting flags.

*   It is illegal if src OR dest is uninitialized.

Affects n, z, and c flags, requiring that they be in the WRITES lists,
and initializing them afterwards.

### and, or, xor ###

    and <dest-memory-location>, <src-memory-location>
    or <dest-memory-location>, <src-memory-location>
    xor <dest-memory-location>, <src-memory-location>

Applies the given bitwise Boolean operation to src and dest and stores
the result in dest.

*   It is illegal if src OR dest OR is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects n and z flags, requiring that they be in the WRITES lists of the
current routine, and sets them as initialized afterwards.

dest and src continue to be initialized afterwards.

### shl, shr ###

    shl <dest-memory-location>
    shr <dest-memory-location>

`shl` shifts the dest left one bit position.  The rightmost position becomes `c`,
and `c` becomes the bit that was shifted off the left.

`shr` shifts the dest right one bit position.  The leftmost position becomes `c`,
and `c` becomes the bit that was shifted off the right.

*   It is illegal if dest is a register besides `a`.
*   It is illegal if dest is read-only.
*   It is illegal if dest OR c is uninitialized.
*   It is illegal if dest does not occur in the WRITES lists
    of the current routine.

Affects the c flag, requiring that it be in the WRITES lists of the
current routine, and it continues to be initialized afterwards.

### call ###

    call <executable-name>

Transfers execution to the given executable, whether that is a previously-
defined routine, or a vector location which contains the address of a routine
which will be called indirectly.  Execution will be transferred back to the
current routine, when execution of the executable is finished.

Just before the call,

*   It is illegal if any of the memory locations in the target executable's
    READS list is uninitialized.

Just after the call,

*   All memory locations listed as TRASHED in the called routine's WRITES
    list are considered uninitialized.
*   All memory locations listed as TRASHED in the called routine's OUTPUTS
    list are considered initialized.

### goto ###

    goto <executable-name>

Unilaterally transfers execution to the given executable.  Execution will not
be transferred back to the current routine when execution of the executable is
finished; rather, it will be transferred back to the caller of the current
routine.

If `goto` is used in a routine, it must be in tail position.  That is, it
must be the final instruction in the routine.

Just before the goto,

*   It is illegal if any of the memory locations in the target executable's
    READS list is uninitialized.

In addition,

*   The target executable's WRITES lists must not include any locations
    that are not already included in the current routine's WRITES lists.

### if ###

    if <src-memory-location> {
        <true-branch>
    } else {
        <false-branch>
    }

Executes the true-branch if the value in src is nonzero, otherwise executes
the false-branch.  The false-branch is optional may be omitted; in this case
it is treated like an empty block.

*   It is illegal if src is not z, c, n, or v.
*   It is illegal if src is not initialized.
*   It is illegal if any location initialized at the end of the true-branch
    is not initialized at the end of the false-branch, and vice versa.

### repeat ###

    repeat {
        <block>
    } until <src-memory-location>

Executes the block repeatedly until the src (observed at the end of the
execution of the block) is non-zero.  The block is always executed as least
once.

*   It is illegal if any memory location is uninitialized at the exit of
    the loop when that memory location is initialized at the start of
    the loop.

To simulate a "while" loop, use an `if` internal to the block, like

    repeat {
        cmp y, 25
        if z {
        }
    } until z

"until" is optional, but if omitted, must be replaced with "forever".

### copy ###

    copy <src-memory-location>, <dest-memory-location>

Reads from src and writes to dest.  Differs from `st` in that is able to
copy more general types of data (for example, vectors,) and it trashes the
`z` and `n` flags and the `a` register.

*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES lists of the current
    routine.
*   It is illegal if src is not of same type as dest.
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized, and `z` and `n`, and
`a` are considered uninitialized.

Grammar
-------

    Program ::= {Defn} {Routine}.
    Defn    ::= Type Ident<new> [Constraints] (":" Literal | "@" LitWord).
    Type    ::= "byte" ["table"] | "vector"
    Constrnt::= ["inputs" LocExprs] ["outputs" LocExprs] ["trashes" LocExprs].
    Routine ::= "routine" Ident<new> Constraints (Block | "@" LitWord).
    LocExprs::= LocExpr {"," LocExpr}.
    LocExpr ::= Register | Flag | Literal | Ident.
    Register::= "a" | "x" | "y".
    Flag    ::= "c" | "z" | "n" | "v".
    Literal ::= LitByte | LitWord.
    LitByte ::= "0" ... "255".
    LitWord ::= "0" ... "65535".
    Block   ::= "{" {Instr} "}".
    Instr   ::= "ld" LocExpr "," LocExpr ["+" LocExpr]
              | "st" LocExpr "," LocExpr ["+" LocExpr]
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
              | "call" Ident<routine>
              | "goto" Ident<executable>
              | "if" ["not"] LocExpr Block ["else" Block]
              | "repeat" Block ("until" ["not"] LocExpr | "forever")
              | "copy" LocExpr "," LocExpr ["+" LocExpr]
              .
