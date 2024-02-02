<!--
Copyright (c) 2014-2024, Chris Pressey, Cat's Eye Technologies.
This file is distributed under a 2-clause BSD license.  See LICENSES/ dir.
SPDX-License-Identifier: LicenseRef-BSD-2-Clause-X-SixtyPical
-->

This directory contains SixtyPical example programs, categorized
in subdirectories by machine architecture.

### rudiments

In the [rudiments](rudiments/) directory are programs which are
meant to demonstrate the elementary features of SixtyPical, and
to serve as manual integration test cases.  See
[the README in that directory](rudiments/README.md) for details.

### c64

In the [c64](c64/) directory are programs that run on the Commodore 64.
The directory itself contains some simple demos, for example
[hearts.60p](c64/hearts.60p), while there are subdirectories for more
elaborate demos, like the flagship demo game.  See
[the README in that directory](c64/README.md) for details.

### vic20

In the [vic20](vic20/) directory are programs that run on the
Commodore VIC-20.  The directory itself contains some simple demos,
for example [hearts.60p](vic20/hearts.60p).

### atari2600

In the [atari2600](atari2600/) directory are programs that run on the
Atari 2600 (4K cartridge).  The directory itself contains a simple
demo, [smiley.60p](atari2600/smiley.60p) which was converted from an
older Atari 2600 skeleton program written in [Ophis][].

### apple2

In the [apple2](apple2/) directory are programs that run on
Apple II series computers (Apple II+, Apple //e).  `sixtypical`'s
support for this architecture could be called embryonic.

[Ophis]: http://michaelcmartin.github.io/Ophis/
