This directory contains SixtyPical example programs, categorized
in subdirectories by the machine architecture.

In the [c64](c64/) directory are programs that run on the Commodore 64:

*   [demo-game](c64/demo-game/): a little game-like program written as a
    "can we write something you'd see in practice?" test case for SixtyPical.
    
*   [ribos](c64/ribos/): a well-commented example of a C64 raster interrupt
    routine. Originally written with the P65 assembler (which has since
    been reborn as [Ophis][]).
    
    The second version of Ribos has been translated to SixtyPical.
    
*   [petulant](c64/petulant/) -- "The PETulant Cursor", a tiny (44 bytes)
    "display hack". Originally written in the late 80's. Rewritten with
    the P65 assembler (now Ophis) and re-released on April 1st, 2008 (a
    hint as to its nature).
    
    Translated to SixtyPical (in 2018), it's 48 bytes.

In the [rudiments](rudiments/) directory are programs which are not for
any particular machine, but meant to demonstrate the features of SixtyPical.
Some are meant to fail and produce an error message.  Others can run on
any architecture where there is a routine at 65490 which outputs the value
of the accumulator as an ASCII character.

[Ophis]: http://michaelcmartin.github.io/Ophis/
