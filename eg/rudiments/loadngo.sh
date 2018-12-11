#!/bin/sh

usage="Usage: loadngo.sh (c64|vic20) <source.60p>"

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
else
  echo $usage && exit 1
fi

src="$1"
if [ "X$src" = "X" ]; then
  echo $usage && exit 1
fi

### do it ###

out=/tmp/a-out.prg
../../bin/sixtypical --traceback --output-format=$output_format support/$arch.60p $src --output $out || exit 1
ls -la $out
$emu $out
rm -f $out
