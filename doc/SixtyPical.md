SixtyPical
==========

This document describes the SixtyPical programming language version 0.8-PRE,
both its execution aspect and its static analysis aspect (even though
these are, technically speaking, separate concepts.)

This document is nominally normative, but the tests in the `tests` directory
are even more normative.

Refer to the bottom of this document for an EBNF grammar of the syntax of
the language.

Types
-----

There are five *primitive types* in SixtyPical:

*   bit (2 possible values)
*   byte (256 possible values)
*   word (65536 possible values)
*   routine (code stored somewhere in memory, read-only)
*   vector (address of a routine)

There is also one *type constructor*:

*   X table (256 entries, each holding a value of type X)

This constructor can only be applied to one type, `byte`.

Memory locations
----------------

A primary concept in SixtyPical is the *memory location*.  At any given point
in time during execution, each memory location is either *uninitialized* or
*initialized*.  At any given point in the program text, too, each memory
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

two hundred and fifty-six byte constants,

    0
    1
    ...
    255

and sixty-five thousand five hundred and thirty-six word constants,

    word 0
    word 1
    ...
    word 65535

Note that if a word constant is between 256 and 65535, the leading `word`
token can be omitted.

### User-defined ###

There may be any number of user-defined memory locations.  They are defined
by giving the type (which may be any type except `bit` and `routine`) and the
name.

    byte pos

An address in memory may be given explicitly on a user-defined memory location.

    byte table screen @ 1024

Or, a user-defined memory location may be given an initial value.  But in this
case, an explicit address in memory cannot be given.

    byte pos : 0

A user-defined vector memory location is decorated with `inputs`, `outputs`
and `trashes` lists like a routine (see below), and it may only hold addresses
of routines which are compatible.  (Meaning, the routine's inputs (resp. outputs,
trashes) must be a subset of the vector's inputs (resp. outputs, trashes.))

    vector actor_logic
      inputs a, score
      outputs x
      trashes y
      @ $c000

Note that in the code of a routine, if a memory location is named by a
user-defined symbol, it is an address in memory, and can be read and written.
But if it is named by a literal integer, either decimal or hexadecimal, it
is a constant and can only be read (and when read always yields that constant
value.  So, for instance, to read the value at `screen` above, in the code,
you would need to reference the symbol `screen`; attempting to read 1024
would not work.

This is actually useful, at least at this point, as you can rely on the fact
that literal integers in the code are always immediate values.  (But this
may change at some point.)

Routines
--------

Every routine must list all the memory locations it *reads from*, which we
call its `inputs`, and all the memory locations it *writes to*.  The latter
we divide into two groups: its `outputs` which it intentionally initializes,
and its `trashes`, which it does not care about, and leaves uninitialized.
For example, if it uses a register to temporarily store an intermediate
value used in a multiplication, that register has no meaning outside of
the multiplication, and is one of the routine's `trashes`.

It is common to say that the `trashes` are the memory locations that are
*not preserved* by the routine.

    routine foo
      inputs a, score
      outputs x
      trashes y {
        ...
    }

The union of the `outputs` and `trashes` is sometimes collectively called
"the WRITES" of the routine, for historical reasons and as shorthand.

Routines may call only routines previously defined in the program source.
Thus, directly recursive routines are not allowed.  (However, routines may
also call routines via vectors, which are dynamically assigned.  In this
case, there is, for the time being, no check for recursive calls.)

For a SixtyPical program to be run, there must be one routine called `main`.
This routine is executed when the program is run.

The memory locations given as inputs to a routine are considered to be initialized
at the beginning of the routine.  Various instructions cause memory locations
to be initialized after they are executed.  Calling a routine which trashes
some memory locations causes those memory locations to be uninitialized after
that routine is called.  At the end of a routine, all memory locations listed
as outputs must be initialized.

A literal word can given instead of the body of the routine.  This word is the
absolute address of an "external" routine located in memory but not defined by
the SixtyPical program.

    routine chrout
      inputs a
      trashes a
      @ 65490

Instructions
------------

Instructions are inspired by, and in many cases closely resemble, the 6502
instruction set.  However, in many cases they do not map 1:1 to 6502 instructions.
If a SixtyPical instruction cannot be translated validly to one more more 6502
instructions while retaining all the stated constraints, that's a static error
in a SixtyPical program, and technically any implementation of SixtyPical, even
an interpreter, should flag it up.

### ld ###

    ld <dest-memory-location>, <src-memory-location> [+ <index-memory-location>]

Reads from src and writes to dest.

*   It is illegal if dest is not a register.
*   It is illegal if dest does not occur in the WRITES of the current routine.
*   It is illegal if src is not of same type as dest (i.e., is not a byte.)
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized.  The flags `z` and `n` may be
changed by this instruction; they must be named in the WRITES, and they
are considered initialized after it has executed.

If and only if src is a byte table, the index-memory-location must be given.

Some combinations, such as `ld x, y`, are illegal because they do not map to
underlying opcodes.

### st ###

    st <src-memory-location>, <dest-memory-location> [+ <index-memory-location>]

Reads from src and writes to dest.

*   It is illegal if dest is a register or if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.
*   It is illegal if src is not of same type as dest.
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized.  No flags are
changed by this instruction (unless of course dest is a flag.)

If and only if dest is a byte table, the index-memory-location must be given.

### copy ###

    copy <src-memory-location>, <dest-memory-location>

Reads from src and writes to dest.  Differs from `st` in that is able to
copy more general types of data (for example, vectors,) and it trashes the
`z` and `n` flags and the `a` register.

*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.
*   It is illegal if src is not of same type as dest.
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized, and `z` and `n`, and
`a` are considered uninitialized.

### add dest, src ###

    add <dest-memory-location>, <src-memory-location>

Adds the contents of src to dest and stores the result in dest.

*   It is illegal if src OR dest OR c is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n, z, c, and v flags, requiring that they be in the WRITES,
and initializing them afterwards.

dest and src continue to be initialized afterwards.

### inc ###

    inc <dest-memory-location>

Increments the value in dest.  Does not honour carry.

*   It is illegal if dest is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n and z flags, requiring that they be in the WRITES,
and initializing them afterwards.

### sub ###

    sub <dest-memory-location>, <src-memory-location>

Subtracts the contents of src from dest and stores the result in dest.

*   It is illegal if src OR dest OR c is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n, z, c, and v flags, requiring that they be in the WRITES,
and initializing them afterwards.

dest and src continue to be initialized afterwards.

### dec ###

    dec <dest-memory-location>

Decrements the value in dest.  Does not honour carry.

*   It is illegal if dest is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n and z flags, requiring that they be in the WRITES,
and initializing them afterwards.

### cmp ###

    cmp <dest-memory-location>, <src-memory-location>

Subtracts the contents of src from dest (without considering carry) but
does not store the result anywhere, only sets the resulting flags.

*   It is illegal if src OR dest is uninitialized.

Affects n, z, and c flags, requiring that they be in the WRITES,
and initializing them afterwards.

### and, or, xor ###

    and <dest-memory-location>, <src-memory-location>
    or <dest-memory-location>, <src-memory-location>
    xor <dest-memory-location>, <src-memory-location>

Applies the given bitwise Boolean operation to src and dest and stores
the result in dest.

*   It is illegal if src OR dest OR is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n and z flags, requiring that they be in the WRITES of the
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
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects the c flag, requiring that it be in the WRITES of the
current routine, and it continues to be initialized afterwards.

### call ###

    call <executable-name>

Transfers execution to the given executable, whether that is a previously-
defined routine, or a vector location which contains the address of a routine
which will be called indirectly.  Execution will be transferred back to the
current routine, when execution of the executable is finished.

*   It is illegal if any of the memory locations listed in the called routine's
    `inputs` are uninitialized immediately before the call.

Just after the call,

*   All memory locations listed in the called routine's `trashes` are considered
    to now be uninitialized.
*   All memory locations listed in the called routine's `outputs` are considered
    to not be initialized.

### goto ###

    goto <executable-name>

Unilaterally transfers execution to the given executable.  Execution will not
be transferred back to the current routine when execution of the executable is
finished; rather, it will be transferred back to the caller of the current
routine.

If `goto` is used in a routine, it must be in tail position.  That is, it
must be the final instruction in the routine.

Just before the goto,

*   It is illegal if any of the memory locations in the target routine's
    `inputs` list is uninitialized.

In addition,

*   The target executable's WRITES must not include any locations
    that are not already included in the current routine's WRITES.

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

The sense of the test can be inverted with `not`.

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

"until" is optional, but if omitted, must be replaced with "forever":

    repeat {
        cmp y, 25
        if z {
        }
    } forever

The sense of the test can be inverted with `not`.

    repeat {
        cmp y, 25
        if z {
        }
    } until not z

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
