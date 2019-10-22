This directory contains example sources which demonstrate
the rudiments of SixtyPical.

Examples that are meant to fail and produce an error message
when being compiled are in the `errorful/` subdirectory.

The other sources are portable across architectures.  They use
`include` directives to bring in architecture-dependent libraries
to produce output.  Libraries for such are provided in the
architecture-specific subdirectories of the `include` directory
in the root directory of this repository; make sure it is on the
compiler's include search path.  For example:

    sixtypical --run-on=x64 -I../../include/c64/:../../include/stdlib/ vector-table.60p

`chrout` is a routine with outputs the value of the accumulator
as an ASCII character, disturbing none of the other registers,
simply for the purposes of producing some observable output.

(There is a KERNAL routine which does this on both the
Commodore 64 and the Commodore VIC-20.  It should not be hard
to find or write such a routine for most other architectures.)
