#!/bin/sh

if [ "X$X64" = "X" ]; then
    X64=x64
fi
SRC=$1
if [ "X$1" = "X" ]; then
    echo "Usage: ./loadngo.sh <source.60p>"
    exit 1
fi
OUT=/tmp/a-out.prg
bin/sixtypical --traceback --basic-prelude $SRC > $OUT || exit 1
ls -la $OUT
if [ -e vicerc ]; then
    $X64 -config vicerc $OUT
else
    $X64 $OUT
fi
rm -f $OUT
