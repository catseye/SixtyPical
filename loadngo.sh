#!/bin/sh

usage="Usage: loadngo.sh (c64|vic20|atari2600|apple2) [--dry-run] <source.60p>"

arch="$1"
shift 1
if [ "X$arch" = "Xc64" ]; then
  output_format='c64-basic-prg'
  if [ -e vicerc ]; then
    emu="x64 -config vicerc"
  else
    emu="x64"
  fi
elif [ "X$arch" = "Xvic20" ]; then
  output_format='vic20-basic-prg'
  if [ -e vicerc ]; then
    emu="xvic -config vicerc"
  else
    emu="xvic"
  fi
elif [ "X$arch" = "Xatari2600" ]; then
  output_format='atari2600-cart'
  emu='stella'
elif [ "X$arch" = "Xapple2" ]; then
  src="$1"
  out=/tmp/a-out.bin
  bin/sixtypical --traceback --origin=0x2000 --output-format=raw $src --output $out || exit 1
  ls -la $out
  cp ~/scratchpad/linapple/res/Master.dsk sixtypical.dsk
  # TODO: replace HELLO with something that does like
  # BLOAD "PROG"
  # CALL 8192
  # (not BRUN because it does not always return to BASIC afterwards not sure why)
  a2rm sixtypical.dsk PROG
  a2in B sixtypical.dsk PROG $out
  linapple -d1 sixtypical.dsk -autoboot
  rm -f $out sixtypical.dsk
  exit 0
else
  echo $usage && exit 1
fi

if [ "X$1" = "X--dry-run" ]; then
  shift 1
  emu='echo'
fi

src="$1"
if [ "X$src" = "X" ]; then
  echo $usage && exit 1
fi

### do it ###

out=/tmp/a-out.prg
bin/sixtypical --traceback --output-format=$output_format $src --output $out || exit 1
ls -la $out
$emu $out
rm -f $out
