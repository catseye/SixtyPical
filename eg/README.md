This directory contains SixtyPical example programs, categorized
in subdirectories by machine architecture.

### rudiments

In the [rudiments](rudiments/) directory are programs which are not for
any particular machine, but meant to demonstrate the features of SixtyPical.
Some are meant to fail and produce an error message.  Others can run on
any architecture where there is a routine at 65490 which outputs the value
of the accumulator as an ASCII character.

### c64

In the [c64](c64/) directory are programs that run on the Commodore 64.
The directory itself contains some simple demos, for example
[hearts.60p](c64/hearts.60p), while there are subdirectories for more
elaborate demos:

*   [demo-game](c64/demo-game/): a little game-like program written as a
    "can we write something you'd see in practice?" test case for SixtyPical.
    
*   [ribos](c64/ribos/): a well-commented example of a C64 raster interrupt
    routine. Originally written with the P65 assembler (which has since
    been reborn as [Ophis][]).
    
    The second version of Ribos has been translated to SixtyPical.
    
*   [petulant](c64/petulant/): "The PETulant Cursor", a tiny (44 bytes)
    "display hack". Originally written in the late 80's. Rewritten with
    the P65 assembler (now Ophis) and re-released on April 1st, 2008 (a
    hint as to its nature).
    
    Translated to SixtyPical (in 2018), it's 48 bytes.

### vic20

In the [vic20](vic20/) directory are programs that run on the
Commodore VIC-20.  The directory itself contains some simple demos,
for example [hearts.60p](vic20/hearts.60p).

### atari2600

In the [atari2600](atari2600/) directory are programs that run on the
Atari 2600 (4K cartridge).  The directory itself contains a simple
demo, [smiley.60p](atari2600/smiley.60p) which was converted from an
older Atari 2600 skeleton program written in [Ophis][].

[Ophis]: http://michaelcmartin.github.io/Ophis/
