History of SixtyPical
=====================

0.5-PRE
-------

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
