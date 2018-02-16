History of SixtyPical
=====================

0.13
----

*   It is a static analysis error if it cannot be proven that a read or write
    to a table falls within the defined size of that table.
*   The reference analyzer's ability to prove this is currently fairly weak,
    but it does exist.

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
