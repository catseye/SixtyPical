SixtyPical
==========

<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

This document describes the SixtyPical programming language version 0.20,
both its static semantics (the capabilities and limits of the static
analyses it defines) and its runtime semantics (with reference to the
semantics of 6502 machine code.)

This document is nominally normative, but the tests in the `tests` directory
are even more normative.

Refer to the bottom of this document for an EBNF grammar of the syntax of
the language.

Data Model
----------

SixtyPical defines a data model where every value has some type
information associated with it.  The values include those that are
directly manipulable by a SixtyPical program, but are not limited to them.
Type information includes not only what kind of structure the data has,
but other properties as well (sometimes called "type annotations".)

### Basic types ###

SixtyPical defines a handful of basic types.  There are three types that
are "primitive" in that they are not parameterized in any way:

*   bit (2 possible values)
*   byte (256 possible values)
*   word (65536 possible values)

Types can also be parameterized and constructed from other types
(which is a kind of parameterization).  One such type constructor is

*   pointer (16-bit address of a byte inside a byte table)
*   vector T (address of a value of type T; T must be a routine type)

Values of the above-listed types are directly manipulable by a SixtyPical
program.  Other types describe values which can only be indirectly
manipulated by a program:

*   routine (code stored somewhere in memory, read-only)
*   T table[N] (series of 1 ≤ N ≤ 65536 values of type T)

There are some restrictions here; for example, a table may only
consist of `byte`, `word`, or `vector` types.  A pointer may only
point to a byte inside a `table` of `byte` type.

Each routine is associated with a rich set of type information,
which is basically the types and statuses of memory locations that
have been declared as being relevant to that routine.

#### User-defined ####

A program may define its own types using the `typedef` feature.  Typedefs
must occur before everything else in the program.  A typedef takes a
type expression and an identifier which has not previously been used in
the program.  It associates that identifer with that type.  This is merely
a type alias; if two types have identical structure but different names,
they will compare as equal.

### Memory locations ###

A primary concept in SixtyPical is the *memory location*.  At any given point
in time during execution, each memory location is either *uninitialized* or
*initialized*.  At any given point in the program text, too, each memory
location is either uninitialized or initialized.  Where-ever it is one or
the other during execution, it is the same in the corresponding place in
the program text; thus, it is a static property.

There are four general kinds of memory location.  The first three are
pre-defined and built-in.

#### Registers ####

Each of these hold a byte.  They are initially uninitialized.

    a
    x
    y

#### Flags ####

Each of these hold a bit.  They are initially uninitialized.

    c (carry)
    z (zero)
    v (overflow)
    n (negative)

#### Constants ####

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

#### User-defined ####

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

    vector routine
             inputs a, score
             outputs x
             trashes y
      actor_logic @ $c000

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

### Tables and Pointers ###

A table is a collection of memory locations that can be indexed in a number
of ways.

The simplest way is to use another memory location as an index.  There
are restrictions on which memory locations can be used as indexes;
only the `x` and `y` locations can be used this way.  Since those can
only hold a byte, this method, by itself, only allows access to the first
256 entries of the table.

    byte table[1024] tab
    ...
    ld a, tab + x
    st a, tab + y

However, by combining indexing with a constant _offset_, entries beyond the
256th entry can be accessed.

    byte table[1024] tab
    ...
    ld a, tab + 512 + x
    st a, tab + 512 + y

Even with an offset, the range of indexing still cannot exceed 256 entries.
Accessing entries at an arbitrary address inside a table can be done with
a `pointer`.  Pointers can only be point inside `byte` tables.  When a
pointer is used, indexing with `x` or `y` will also take place.

A `pointer` is implemented as a zero-page memory location, and accessing the
table pointed to is implemented with "indirect indexed" addressing, as in

    LDA ($02), Y
    STA ($02), Y

There are extended instruction modes for using these types of memory location.
See `copy` below, but here is some illustrative example code:

    point ptr into buf {     // this associates this pointer with this table
      reset ptr 0            // this is the only way to initialize a pointer
      add ptr, 4             // note, this is unchecked against table's size!
      ld y, 0                // you must set this to something yourself
      copy [ptr] + y, byt    // read memory through pointer, into byte
      copy 100, [ptr] + y    // write memory through pointer (still trashes a)
    }                        // after this block, ptr can no longer be used

where `ptr` is a user-defined storage location of `pointer` type, `buf`
is a `table` of `byte` type, and the `+ y` part is mandatory.

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
In this case, it is illegal if the value of the index-memory-location falls
outside of the range of the table.

Some combinations, such as `ld x, y`, are illegal because they do not map to
underlying opcodes.  (For an instruction which maps more flexibly to underlying
opcodes, see `copy`.)

There is another mode of `ld` which reads into `a` indirectly through a pointer.

    ld a, [<src-memory-location>] + y

The memory location in this syntax must be a pointer.

This syntax copies the contents of memory at the pointer (offset by the `y`
register) into a register (which must be the `a` register.)

In addition to the constraints above, `y` must be initialized before
this mode is used.

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
In this case, it is illegal if the value of the index-memory-location falls
outside of the range of the table.

There is another mode of `st` which write `a` into memory, indirectly through
a pointer.

    st a, [<dest-memory-location>] + y

The memory location in this syntax must be a pointer.

This syntax copies the constents of the `a` register into
the contents of memory at the pointer (offset by the `y` register).

In addition to the constraints above, `y` must be initialized before
this mode is used.

### copy ###

    copy <src-memory-location>, <dest-memory-location>

Reads from src and writes to dest.  Differs from `ld` and `st` in that
it is able to copy more general types of data (for example, vectors,)
and it trashes the `z` and `n` flags and the `a` register.

*   It is illegal if dest is read-only.
*   It is illegal if dest does not occur in the WRITES of the current routine.
*   It is illegal if src is not of same type as dest.
*   It is illegal if src is uninitialized.

After execution, dest is considered initialized, and `z` and `n`, and
`a` are considered uninitialized.

There is an extra mode that this instruction can be used in:

    copy [<src-memory-location>] + y, <dest-memory-location>
    copy <src-memory-location>, [<dest-memory-location>] + y

In both of these, the memory location in the `[]+y` syntax must be
a pointer.

The first copies the contents of memory at the pointer (offset by the `y`
register) into a byte memory location.

The second copies a literal byte, or a byte memory location, into
the contents of memory at the pointer (offset by the `y` register).

In addition to the constraints above, `y` must be initialized before
this mode is used.

### add dest, src ###

    add <dest-memory-location>, <src-memory-location>

Adds the contents of src to dest and stores the result in dest.

*   It is illegal if src OR dest OR `c` is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest is `x` or `y`.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n, z, c, and v flags, requiring that they be in the WRITES,
and initializing them afterwards.

dest and src continue to be initialized afterwards.

In addition, if dest is of `word` type, then src must also be of `word`
type, and in this case this instruction trashes the `a` register.

In fact, this instruction trashes the `a` register in all cases except
when the dest is `a`.

NOTE: If dest is a pointer, the addition does not check if the result of
the pointer arithmetic continues to be valid (within a table) or not.

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

*   It is illegal if src OR dest OR `c` is uninitialized.
*   It is illegal if dest is read-only.
*   It is illegal if dest is `x` or `y`.
*   It is illegal if dest does not occur in the WRITES of the current routine.

Affects n, z, c, and v flags, requiring that they be in the WRITES,
and initializing them afterwards.

dest and src continue to be initialized afterwards.

In addition, if dest is of `word` type, then src must also be of `word`
type, and in this case this instruction trashes the `a` register.

In fact, this instruction trashes the `a` register in all cases except
when the dest is `a`.

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
This means that `z` is set if src and dest are equal,
and `c` is set if dest is greater than or equal to src
(`c` is unset if dest is less than src.)

*   It is illegal if src OR dest is uninitialized.

Affects n, z, and c flags, requiring that they be in the WRITES,
and initializing them afterwards.

In addition, if dest is of `word` type, then src must also be of `word`
type, and in this case this instruction trashes the `a` register.

Note that `cmp` is not suitable for making a
signed comparison; this article, which mentions
techniques that a SixtyPical compiler could use to
implement `cmp`, also explains why that is:
[Beyond 8-bit Unsigned Comparisons, by Bruce Clark](http://www.6502.org/tutorials/compare_beyond.html).

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
    to now be initialized.

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

### for ###

    for <dest-memory-location> (up|down) to <literal-byte> {
        <block>
    }

Executes the block repeatedly, incrementing or decrementing the
dest-memory-location at the end of the block, until the value of
the dest-memory-location has gone past the literal-byte.

The block is always executed as least once.

*   It is illegal if any memory location is uninitialized at the exit of
    the loop when that memory location is initialized at the start of
    the loop.

Grammar
-------

    Program ::= {ConstDefn | TypeDefn} {Defn} {Routine}.
    ConstDefn::= "const" Ident<new> Const.
    TypeDefn::= "typedef" Type Ident<new>.
    Defn    ::= Type Ident<new> (":" Const | "@" LitWord).
    Type    ::= TypeTerm ["table" TypeSize].
    TypeExpr::= "byte"
              | "word"
              | "pointer"
              | "vector" TypeTerm
              | "routine" Constraints
              | "(" Type ")"
              .
    TypeSize::= "[" LitWord "]".
    Constrnt::= ["inputs" LocExprs] ["outputs" LocExprs] ["trashes" LocExprs].
    Routine ::= "define" Ident<new> Type (Block | "@" LitWord).
    LocExprs::= LocExpr {"," LocExpr}.
    LocExpr ::= Register | Flag | Const | Ident [["+" Const] "+" Register].
    Register::= "a" | "x" | "y".
    Flag    ::= "c" | "z" | "n" | "v".
    Const   ::= Literal | Ident<const>.
    Literal ::= LitByte | LitWord | LitBit.
    LitByte ::= "0" ... "255".
    LitWord ::= ["word"] "0" ... "65535".
    LitBit  ::= "on" | "off".
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
              | "copy" LocExpr "," LocExpr ["+" LocExpr]
              | "if" ["not"] LocExpr Block ["else" Block]
              | "repeat" Block ("until" ["not"] LocExpr | "forever")
              | "for" LocExpr ("up"|"down") "to" Const Block
              | "with" "interrupts" LitBit Block
              | "point" LocExpr "into" LocExpr Block
              | "reset" LocExpr Const
              .
