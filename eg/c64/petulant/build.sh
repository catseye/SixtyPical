#!/bin/sh

sixtypical --output-format=prg --origin=0x02a7 petulant.60p > petulant-60p.prg
if [ "x$COMPARE" != "x" ]; then
  dcc6502 petulant.prg > petulant.prg.disasm.txt
  dcc6502 petulant-60p.prg > petulant-60p.prg.disasm.txt
  paste petulant.prg.disasm.txt petulant-60p.prg.disasm.txt | pr -t -e24
  rm -f *.disasm.txt
fi
