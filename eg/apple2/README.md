<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

This directory contains SixtyPical example programs
specifically for the Apple II series of computers.

See the [README in the parent directory](../README.md) for
more information on these example programs.

Note that `sixtypical` does not currently support "load
and go" execution of these programs, because constructing
an Apple II disk image file on the fly is not something
it can currently do.  If you have the linapple sources
checked out, and the a2tools available, you could do
something like this:

    bin/sixtypical --traceback --origin=0x2000 --output-format=raw eg/apple2/prog.60p --output prog.bin
    cp /path/to/linapple/res/Master.dsk sixtypical.dsk
    a2rm sixtypical.dsk PROG
    a2in B sixtypical.dsk PROG prog.bin
    linapple -d1 sixtypical.dsk -autoboot

and then enter

    BLOAD PROG
    CALL 8192

Ideally you could

    BRUN PROG

But that does not always return to BASIC and I'm not sure why.
