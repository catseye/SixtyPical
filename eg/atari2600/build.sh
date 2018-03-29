#!/bin/sh

sixtypical --prelude=atari2600 smiley.60p > smiley-60p.bin
if [ "x$COMPARE" != "x" ]; then
  ophis smiley.oph -o smiley.bin
  dcc6502 -o 0xf000 -m 200 smiley.bin > smiley.bin.disasm.txt
  dcc6502 -o 0xf000 -m 200 smiley-60p.bin > smiley-60p.bin.disasm.txt
  paste smiley.bin.disasm.txt smiley-60p.bin.disasm.txt | pr -t -e24
fi
