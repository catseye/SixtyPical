#!/bin/sh

SRC=$1
OUT=/tmp/a-out.prg
bin/sixtypical --analyze --compile --basic-prelude $SRC > $OUT || exit 1
x64 $OUT
rm -f $OUT
