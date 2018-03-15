#!/bin/sh

sixtypical --output-format=prg --origin=0xc000 ribos2.60p > ribos2-60p.prg
if [ "x$COMPARE" != "x" ]; then
  dcc6502 ribos2.prg > ribos2.prg.disasm.txt
  dcc6502 ribos2-60p.prg > ribos2-60p.prg.disasm.txt
  paste ribos2.prg.disasm.txt ribos2-60p.prg.disasm.txt | pr -t -e24
  rm -f *.disasm.txt
fi
