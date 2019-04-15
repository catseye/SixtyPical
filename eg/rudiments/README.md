This directory contains example sources which demonstrate
the rudiments of SixtyPical.

Examples that are meant to fail and produce an error message
are in the `errorful/` subdirectory.

These files are intended to be architecture-agnostic.
For the ones that do produce output, an appropriate source
under `support/` should be included first, so that system entry
points such as `chrout` are defined.  In addition, some of these
programs use "standard" support modules, so those should be included
first too.  For example:

    sixtypical --output-format=c64-basic-prg --run support/c64.60p support/stdlib.60p vector-table.60p

`chrout` is a routine with outputs the value of the accumulator
as an ASCII character, disturbing none of the other registers,
simply for the purposes of producing some observable output.

(There is a KERNAL routine which does this on both the
Commodore 64 and the Commodore VIC-20.  It should not be hard
to find or write such a routine for most other architectures.)
