The PETulant Cursor
===================

This is a tiny (44 bytes long) machine-language demo for the Commodore 64,
somewhat in the style of later "display hacks" for the Amiga -- surprising
and silly ways to play with the user interface.

So as not to not spoil the fun, try running it before reading the source.

To run it, make the file PETULANT.PRG accessible to your favourite Commodore
64 (or Commodore 64 emulator) in your favourite way, then

  LOAD "PETULANT.PRG",8,1
  SYS 679

For further fun, try changing the text colour (hold the C= or CTRL key while
pressing a number) or the background colour (POKE 53281 with a number from
0 to 15) while it is running.

I must have originally wrote this sometime in the late 80's.  I disassembled
and rewrote it (to make it play more nicely with existing interrupt handlers)
in 2008.

Who knows -- this could actually be useful, in a ridiculously minor way: it
makes it much more obvious when control has returned to BASIC immediate mode
(e.g., when a program has finished loading.)

Enjoy!

-Chris Pressey
April 1, 2008
Chicago, IL
