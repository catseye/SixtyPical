#!/bin/sh

usage="Usage: loadngo.sh (c64|vic20|atari2600) [--dry-run] <source.60p>"

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
bin/sixtypical --traceback --output-format=$output_format $src > $out || exit 1
ls -la $out
$emu $out
rm -f $out
