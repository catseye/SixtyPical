#!/bin/sh

usage="Usage: loadngo.sh (c64|vic20) [--dry-run] <source.60p>"

arch="$1"
shift 1
if [ "X$arch" = "Xc64" ]; then
  prelude='c64'
  if [ -e vicerc ]; then
    emu="x64 -config vicerc"
  else
    emu="x64"
  fi
elif [ "X$arch" = "Xvic20" ]; then
  prelude='vic20'
  if [ -e vicerc ]; then
    emu="xvic -config vicerc"
  else
    emu="xvic"
  fi
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
bin/sixtypical --traceback --prelude=$prelude $src > $out || exit 1
ls -la $out
$emu $out
rm -f $out
