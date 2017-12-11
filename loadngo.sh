#!/bin/sh

SRC=$1
OUT=/tmp/a-out.prg
bin/sixtypical --traceback --analyze --compile --basic-prelude $SRC > $OUT || exit 1
if [ -e vicerc ]; then
    x64 -config vicerc $OUT
else
    x64 $OUT
fi
rm -f $OUT
