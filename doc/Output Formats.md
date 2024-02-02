Output Formats
==============

<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

`sixtypical` can generate an output file in a number of formats.

### `raw`

The file contains only the emitted bytes of the compiled SixtyPical
program.

The default origin is $0000; you will likely want to override this.

Note that the origin is not stored in the output file in this format;
that information must be recorded separately.

### `prg`

The first two bytes of the file contain the origin address in
little-endian format.  The remainder of the file is the emitted bytes
of the compiled SixtyPical program, starting at that origin.

The default origin is $C000; you will likely want override this.

This format coincides with Commodore's PRG format for disk files,
thus its name.

### `c64-basic-prg`

The first few bytes of the file contain a short Commodore 2.0 BASIC
program.  Directly after this is the emitted bytes of the compiled
SixtyPical program.  The BASIC program contains a `SYS` to that code.

The default origin is $0801; it is unlikely that you will want to
override this.

This format allows the PRG file to be loaded and run on a Commodore 64
with

    LOAD"FOO.PRG",8:RUN

### `vic20-basic-prg`

Exactly like `--c64-basic-prg` except intended for the Commodore VIC-20.

The default origin is $1001; it is unlikely that you will want to
override this.

This format allows the PRG file to be loaded and run on a VIC-20 with

    LOAD"FOO.PRG",8:RUN

### `atari2600-cart`

The file starts with a short machine-language prelude which is intended
to initialize an Atari 2600 system, followed by the emitted bytes of the
compiled SixtyPical program.

The file is padded to 4096 bytes in length.  The padding is mostly
zeroes, except for the final 4 bytes of the file, which consist of
two addresses in little-endian format; both are the origin address.

The default origin is $F000; it is unlikely you will want to
override this.

This is the format used by Atari 2600 cartridges.
