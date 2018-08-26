SixtyPical
==========

_Version 0.17.  Work-in-progress, everything is subject to change._

**SixtyPical** is a 6502-like programming language with advanced
static analysis.

"6502-like" means that it has similar restrictions as programming
in 6502 assembly (e.g. the programmer must choose the registers that
values will be stored in) and is concomitantly easy for a compiler to
translate it to 6502 machine language code.

"Advanced static analysis" includes _abstract interpretation_, where we
go through the program step by step, tracking not just the changes that
happen during a _specific_ execution of the program, but _sets_ of changes
that could _possibly_ happen in any run of the program.  This lets us
determine that certain things can never happen, which we can then formulate
as safety checks.

In practice, this means it catches things like

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you call trashes a register you thought was preserved
*   you tried to read or write a byte beyond the end of a byte array
*   you tried to write the address of something that was not a routine, to
    a jump vector

and suchlike.  It also provides some convenient operations based on
machine-language programming idioms, such as

*   copying values from one register to another (via a third register when
    there are no underlying instructions that directly support it); this
    includes 16-bit values, which are copied in two steps
*   explicit tail calls
*   indirect subroutine calls

The reference implementation can analyze and compile SixtyPical programs to
6502 machine code.

Quick Start
-----------

If you have the [VICE][] emulator installed, from this directory, you can run

    ./loadngo.sh c64 eg/c64/hearts.60p

and it will compile the [hearts.60p source code](eg/c64/hearts.60p) and
automatically start it in the `x64` emulator, and you should see:

![Screenshot of result of running hearts.60p](images/hearts.png?raw=true)

You can try the `loadngo.sh` script on other sources in the `eg` directory
tree, which contains more extensive examples, including an entire
game(-like program); see [eg/README.md](eg/README.md) for a listing.

[VICE]: http://vice-emu.sourceforge.net/

Documentation
-------------

*   [Design Goals](doc/Design%20Goals.md)
*   [SixtyPical specification](doc/SixtyPical.md)
*   [SixtyPical revision history](HISTORY.md)
*   [Literate test suite for SixtyPical syntax](tests/SixtyPical%20Syntax.md)
*   [Literate test suite for SixtyPical analysis](tests/SixtyPical%20Analysis.md)
*   [Literate test suite for SixtyPical compilation](tests/SixtyPical%20Compilation.md)
*   [Literate test suite for SixtyPical fallthru optimization](tests/SixtyPical%20Fallthru.md)
*   [6502 Opcodes used/not used in SixtyPical](doc/6502%20Opcodes.md)
*   [Output formats supported by `sixtypical`](doc/Output%20Formats.md)
*   [TODO](TODO.md)
