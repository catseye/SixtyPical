History of SixtyPical
=====================

0.9
---

*   Add word (constant or memory location) to word memory location.
*   Add word to pointer (unchecked for now).
*   Implementation: `--debug` shows some extra info during analysis.

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
