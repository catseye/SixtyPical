SixtyPical
==========

_Version 0.17.  Work-in-progress, everything is subject to change._

**SixtyPical** is a low-level programming language with advanced
static analysis.  Many of its primitive instructions resemble
those of the 6502 CPU — in fact it is intended to be compiled to
6502 machine code — but along with these instructions are
constructs which ease structuring and analyzing the code.  The
language aims to fill this niche:

*   You'd use assembly, but you don't want to spend hours
    debugging (say) a memory overrun that happened because of a
    ridiculous silly error.
*   You'd use C or some other "high-level" language, but you don't
    want the extra overhead added by the compiler to manage the
    stack and registers.

SixtyPical gives the programmer a coding regimen on par with assembly
language in terms of size and hands-on-ness, but also able to catch
many ridiculous silly errors at compile time, such as

*   you forgot to clear carry before adding something to the accumulator
*   a subroutine that you called trashes a register you thought it preserved
*   you tried to read or write a byte beyond the end of a byte array
*   you tried to write the address of something that was not a routine, to
    a jump vector

Many of these checks are done with _abstract interpretation_, where we
go through the program step by step, tracking not just the changes that
happen during a _specific_ execution of the program, but _sets_ of changes
that could _possibly_ happen in any run of the program.

SixtyPical also provides some convenient operations based on
machine-language programming idioms, such as

*   copying values from one register to another (via a third register when
    there are no underlying instructions that directly support it); this
    includes 16-bit values, which are copied in two steps
*   explicit tail calls
*   indirect subroutine calls

SixtyPical is defined by a specification document, a set of test cases,
and a reference implementation written in Python 2.  The reference
implementation can analyze and compile SixtyPical programs to 6502 machine
code, which can be run on several 6502-based 8-bit architectures:

*   Commodore 64
*   Commodore VIC-20
*   Atari 2600 VCS
*   Apple II

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
