This directory contains example sources which demonstrate
the rudiments of SixtyPical.

Examples that are meant to fail and produce an error message
are in the `errorful/` subdirectory.

They are not meant to be specific to any architecture, but
many do assume the existence of a routine at 65490 which
outputs the value of the accumulator as an ASCII character,
simply for the purposes of producing some observable output.
(This is an address of a KERNAL routine which does this
on both the Commodore 64 and the Commodore VIC-20, so these
sources should be usable on these architectures.)
