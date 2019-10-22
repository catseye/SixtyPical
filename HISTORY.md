History of SixtyPical
=====================

0.21
----

*   A source file can be included in another source file
    by means of the `include` directive.
*   A routine can be declared `preserved`, which prevents a
    compiler from omitting it from the final executable, even
    if it determines it is not called by any other routine.
*   The reference implementation constructs a callgraph and
    determines the set of routines which are not reachable
    (directly or indirectly) from `main`, with an eye to
    omitting them from the final executable.
*   Added `--prune-unreachable-routines` option, which causes
    the compiler to in fact omit routines determined to be
    unreachable as described above.
*   Added `--include-path` option, which configures the list
    of directories that are searched when a source file is
    included with the `include` directive.
*   Code generation now performs modest peephole optimization
    at the end of each routine.  This results in better code
    generation for constructs in tail position, notably
    tail optimization of `calls`, but also for `goto`s and
    `if` blocks at the end of a routine.

0.20
----

*   A `point ... into` block no longer initializes the pointer
    by default.  A subequent `reset` instruction must be used
    to initialize the pointer.  The pointer may be reset to any
    valid offset within the table (not only 0) and it may be
    reset multiple times inside the block.
*   Local locations need no longer be static.  If they are not
    static, they are considered uninitialized until assigned,
    and they can be declared with an explicit fixed address.
*   Along with `goto`, `call` and `with interrupts off` are
    now forbidden inside a `with interrupts off` block.
*   More tests to assure that using `call` inside a `point into`
    block or inside a `for` block does not cause trouble,
    particularly when the routine being called also uses the
    variable named in that block.
*   Fixed a bug where two local statics could be declared with
    the same name.
*   Split analysis context support off from analyzer, and
    symbol table support from parse, and it their own modules.
*   Split the SixtyPical Analysis tests across three files,
    and placed test appliances for `sixtypical` in own file.

0.19
----

*   A `table` may be defined with more than 256 entries, even
    though the conventional index syntax can only refer to the
    first 256 entries.
*   A `pointer` may point inside values of type `byte table`,
    allowing access to entries beyond the 256th.
*   `buffer` types have been eliminated from the language,
    as the above two improvements allow `byte table`s to
    do everything `buffer`s previously did.
*   When accessing a table with an index, a constant offset
    can also be given.
*   Accessing a `table` through a `pointer` must be done in
    the context of a `point ... into` block.  This allows the
    analyzer to check *which* table is being accessed.
*   Refactored compiler internals so that type information
    is stored in a single symbol table shared by all phases.
*   Refactored internal data structures that represent
    references and types to be immutable `namedtuple`s.
*   Added `--dump-exit-contexts` option to `sixtypical`.
*   Added a new `--run-on=<emu>` option to `sixtypical`, which
    replaces the old `loadngo.sh` script.

0.18
----

*   The "consistent initialization" check inside `if` blocks has
    been dropped.  If a location is initialized inside one block
    but not the other, it is treated as uninitialized afterwards.
*   Syntactically, `goto` may only appear at the end of a block.
    It need no longer be the final instruction in a routine,
    as long as the type context is consistent at every exit.
*   When the range of a location is known, `inc` and `dec`
    on it will usually shift the known instead of invalidating it.
*   `cmp` instruction can now perform a 16-bit unsigned comparison
    of `word` memory locations and `word` literals (at the cost of
    trashing the `a` register.)
*   `add` (resp. `sub`) now support adding (resp. subtracting) a
    byte location or a byte literal from a byte location.
*   Fixed pathological memory use in the lexical scanner - should
    be much less inefficient now when parsing large source files.
*   Reorganized the examples in `eg/rudiments/` to make them
    officially platform-agnostic and to state the expected output.

0.17
----

*   `save X, Y, Z { }` now allowed as a shortcut for nested `save`s.
*   If the name in a location expression isn't found in the symbol
    table, a forward reference will _always_ be generated; and forward
    references in _all_ operations will be resolved after parsing.
*   As a consequence, trying to call or goto a non-routine-typed symbol
    is now an analysis error, not a syntax error.
*   Deprecated `routine foo ...` syntax has been removed.
*   Split TODO off into own file.
*   `sixtypical` no longer writes the compiled binary to standard
    output.  The `--output` command-line argument should be given
    to get a compiled binary; otherwise only analysis is run.
*   Internal cleanups, including a hierarchy of `Outputters`.
*   All tests pass when `sixtypical` is run under Python 3.5.2.

0.16
----

*   Added `save` block, which allows the named locations to be modified
    arbitrarily inside the block, and automatically restored at the end.
*   More thorough tests and justifications written for the case of
    assigning a routine to a vector with a "wider" type.
*   Support for `copy [ptra]+y, [ptrb]+y` to indirect LDA indirect STA.
*   Support for `shl foo` and `shr foo` where `foo` is a byte storage.
*   Support for `I a, btable + x` where `I` is `add`, `sub`, `cmp`,
    `and`, `or`, or `xor`
*   Support for `I btable + x` where `I` is `shl`, `shr`, `inc`, `dec`
*   `or a, z`, `and a, z`, and `eor a, z` compile to zero-page operations
    if the address of z < 256.
*   Removed `--prelude` in favour of specifying both format and prelude
    with a single option, `--output-format`.  Documentation for same.

0.15
----

*   Symbolic constants can be defined with the `const` keyword, and can
    be used in most places where literal values can be used.
*   Added `nop` opcode, which compiles to `NOP` (mainly for timing.)
*   Accessing zero-page with `ld` and `st` generates zero-page opcodes.
*   A `byte` or `word` table can be initialized with a list of constants.
*   Branching and repeating on the `n` flag is now supported.
*   The `--optimize-fallthru` option causes the routines of the program
    to be re-ordered to maximize the number of cases where a `goto`'ed
    routine can be simply "falled through" to instead of `JMP`ed to.
*   `--dump-fallthru-info` option outputs the information from the
    fallthru analysis phase, in JSON format, to stdout.
*   Even without fallthru optimization, `RTS` is no longer emitted after
    the `JMP` from compiling a final `goto`.
*   Specifying multiple SixtyPical source files will produce a single
    compiled result from their combination.
*   Rudimentary support for Atari 2600 prelude in a 4K cartridge image,
    and an example program in `eg/atari2600` directory.

0.14
----

*   Added the so-called "open-faced `for` loop", which spans a loop
    variable over a finite range, the end of which is fixed.
*   "Tail position" is now more correctly determined for the purposes of
    insisting that `goto` only appears in it.
*   New `--origin` and `--output-format` options added to the compiler.
*   Fixed bug when `--prelude` option was missing.
*   Fixed bug when reporting line numbers of scanner-level syntax errors.
*   Translated the small demo projects Ribos and "The PETulant Cursor" to
    SixtyPical, and added them to the `eg/c64/` section of the repo.
*   Added a `eg/vic20` example directory, with one VIC-20 example program.

0.13
----

*   It is a static analysis error if it cannot be proven that a read or write
    to a table falls within the defined size of that table.
*   The reference analyzer's ability to prove this is currently fairly weak,
    but it does exist:
    *   Loading a constant into a memory location means we know the range
        is exactly that one constant value.
    *   `AND`ing a memory location with a value means the range of the
        memory location cannot exceed the range of the value.
    *   Doing arithmetic on a memory location invalidates our knowledge
        of its range.
    *   Copying a value from one memory location to another copies the
        known range as well.
*   Cleaned up the internals of the reference implementation (incl. the AST)
    and re-organized the example programs in the `eg` subdirectory.
*   Most errors produced by the reference implementation now include a line number.
*   Compiler supports multiple preludes, specifically both Commodore 64 and
    Commodore VIC-20; the `loadngo.sh` script supports both architectures too.

0.12
----

*   `copy` is now understood to trash `a`, thus it is not valid to use `a` in `copy`.
    To compensate, indirect addressing is supported in `ld` and `st`, for example,
    as `ld a, [ptr] + y` and `st a, [ptr] + y`.
*   Implements the "union rule for trashes" when analyzing `if` blocks.
*   Even if we `goto` another routine, we can't trash an output.
*   `static` storage locations local to routines can now be defined within routines.
*   Small grammar changes that obviate the need for:
    *   the parentheses in type expressions like `vector (routine ...) table[256]`
    *   the `forward` keyword in forward references in source of `copy` instruction
*   Fixed bug where `trash` was not marking the location as being virtually altered.

0.11
----

*   Each table has a specified size now (although, bounds checking is not performed.)
*   Initialized `byte table` values need not have all 256 bytes initialized.
*   Syntax for types has changed. `routine` (with constraints) is a type, while
    `vector` is now a type constructor (taking `routine`s only) and `table` is
    also a type constructor.  This permits a new `vector table` type.
*   Added `typedef`, allowing the user to define type aliases for readability.
*   Added `define name routine {...}` syntax; `routine name {...}` is now legacy.
*   Ability to copy vectors and routines into vector tables, and vectors out of same.
*   Removed the evaluator.  The reference implementation only analyzes and compiles.
*   Fixed bug where index register wasn't required to be initialized before table access.
*   Fixed bug where trampolines for indirect calls weren't including a final `RTS`.

0.10
----

*   Can `call` and `goto` routines that are defined further down in the source code.
*   The `forward` modifier can also be used to indicate that the symbol being copied
    in a `copy` to a vector is a routine that is defined further down in the source.
*   Initialized `word` memory locations.
*   Can `copy` a literal word to a word table.
*   Subtract word (constant or memory location) from word memory location.
*   `trash` instruction explicitly indicates a value is no longer considered meaningful.
*   `copy []+y, a` can indirectly read a byte value into the `a` register.
*   Initialized `byte table` memory locations.
*   Fixed bug which was preventing `if` branches to diverge in what they initialized,
    if it was already initialized when going into the `if`.
*   Fixed a bug which was making it crash when trying to analyze `repeat forever` loops.

0.9
---

*   Add word (constant or memory location) to word memory location.
*   Add word to pointer (unchecked for now).
*   Added `word table` type.
*   Can `copy` from word storage location to word table and back.
*   A `vector` can name itself in its `inputs` and `outputs` or `trashes` sets.
*   Implementation: `--debug` shows some extra info during analysis.
*   Fixed bug where `copy`ing literal word into word storage used wrong endianness.
*   Fixed bug where every memory location was allocated 2 bytes of storage, regardless of type.
*   Tests: use https://github.com/tcarmelveilleux/dcc6502 to disassemble code for comparison.

0.8
---

*   Explicit word literals prefixed with `word` token.
*   Can `copy` literals into user-defined destinations.
*   Fixed bug where loop variable wasn't being checked at end of `repeat` loop.
*   `buffer` and `pointer` types.
*   `copy ^` syntax to load the addr of a buffer into a pointer.
*   `copy []+y` syntax to read and write values to and from memory through a pointer.

0.7
---

*   User-defined `byte` locations can be given an initial value.
*   `word` type locations which can be defined and `copy`ed between.
*   Can `copy` directly from one user-defined `byte` location to another.

0.6
---

*   Added `routine` and `vector` types, and `copy` instruction.
*   Both routines and vectors can declare `inputs`, `outputs`, and `trashes`,
    and these must be compatible to assign a routine or vector to a vector.
*   Added `goto` (tail call) instruction, jumps to routine or through vector.
*   `call` can call a subroutine indirectly, via a vector.
*   Routine name is now shown in analysis error messages.

0.5
---

*   Added `byte table` type locations and indexed addressing (`+ x`, `+ y`).
*   Integer literals may be given in hexadecimal.
*   Line comments may be included in source code by prefixing them with `//`.

0.4
---

*   Added `repeat` loops to the language, which can repeat until a flag
    is set (or `not` set), or which can repeat `forever`.
*   `if not` inverts the sense of the test.
*   Added explicitly-addressed memory locations.

0.3
---

*   Added external routine declarations.
*   Added ability to compile to 6502 machine code and output a `PRG` file.

0.2
---

A complete reboot of SixtyPical 0.1.  The reference implementation was
rewritten in Python.  The language was much simplified.  The aim was to get the
analysis completely right before adding more sophisticated and useful features
in future versions.

0.1
---

Initial inspired-but-messy version implemented in Haskell.
