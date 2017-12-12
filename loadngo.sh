#!/bin/sh

SRC=$1
if [ "X$1" = "X" ]; then
    echo "Usage: ./loadngo.sh <source.60p>"
    exit 1
fi
OUT=/tmp/a-out.prg
bin/sixtypical --traceback --analyze --compile --basic-prelude $SRC > $OUT || exit 1
if [ -e vicerc ]; then
    x64 -config vicerc $OUT
else
    x64 $OUT
fi
rm -f $OUT
