#!/bin/sh

sixtypical --prelude=atari2600 atari-2600-example.60p > atari-2600-example-60p.bin
if [ "x$COMPARE" != "x" ]; then
  ophis atari-2600-example.oph -o atari-2600-example.bin
  dcc6502 -o 0xf000 -m 200 atari-2600-example.bin > atari-2600-example.bin.disasm.txt
  dcc6502 -o 0xf000 -m 200 atari-2600-example-60p.bin > atari-2600-example-60p.bin.disasm.txt
  paste atari-2600-example.bin.disasm.txt atari-2600-example-60p.bin.disasm.txt | pr -t -e24
  #diff -ru atari-2600-example.bin.disasm.txt atari-2600-example-60p.bin.disasm.txt
fi
