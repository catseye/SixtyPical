Ribos
=====

<!--
SPDX-FileCopyrightText:  Chris Pressey, the author of this work, has dedicated it to the public domain.
For more information, please refer to <https://unlicense.org/>
SPDX-License-Identifier: Unlicense
-->

This little demo is intended to be a well-commented example of how to
program a raster interrupt in 6502 assembly language on a Commodore 64.

This (r)aster (i)nterrupt changes the colour of a region of the
(bo)rder of the C64 (s)creen; thus, RIBOS.  Also, it's the name of a
planet from Dr. Who, if that means anything.


How to Run the Demo (using the VICE C64 emulator, x64)
------------------------------------------------------

0. Obtain VICE from http://www.viceteam.org/, install it,
   and run x64

1. Mount this project's directory as drive 8:

   Make sure
     `Peripheral settings > Device #8 > Enable IEC Device`
   is checked, then select
     `Peripheral settings > Device #8 > File system directory...`
   and enter the path to the project directory.

2. LOAD "RIBOS.PRG",8,1

3. SYS 49152

4. You should see the colour of the middle of the border change
   while you get a READY. prompt and can continue working.


How to Assemble the Program (using the p65 assembler)
-----------------------------------------------------

0. Obtain p65 from http://hkn.berkeley.edu/~mcmartin/P65/
   (I used p65-Perl version 1.1) and install it somewhere
   on your path.  If your Perl interpreter isn't located at
   /usr/bin/perl, change the first line of p65 appropriately.

1. p65 -v -t -b ribos.p65 ribos.prg

   The switches aren't necessary, but they make it feel like
   p65 is doing something difficult and important.  It also
   isn't necessary to add the '.prg' extension on the end of
   the binary object's filename, since it will appear as a
   PRG file to VICE anyway, but it's nice as a reminder when
   you're working in a modern operating system.

2. Follow the steps under 'How to Run this Demo' to see that
   it worked.


How it Works
------------

Read the source!  I've tried to make it very well-commented,
including what happens when you leave out some steps.

I wrote this demo because it was a long time since I had done any C64
programming, and, having just obtained a copy of the 'Commodore 64
Programmer's Reference Guide,' I wanted to code something challenging,
yet not too involved.  I remembered raster interrupts as one of those
quintessential C64 low-level graphics tricks, so I decided to try my
hand at that.  Looking around on the Internet, I found this page:

  http://everything2.com/index.pl?node_id=79254

Although it's a fairly detailed description, it took me a couple of
frustrating hours to implement it successfully - both the everything2
article and the Reference Guide were pretty muddy on a couple of
points.  What I learned in the process is written into the comments.

Happy raster-interrupting!

-Chris Pressey  
April 10, 2007  
Vancouver, BC  
