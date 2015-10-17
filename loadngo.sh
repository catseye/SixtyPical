#!/bin/sh

SRC=$1
OUT=/tmp/a-out.prg
bin/sixtypical --analyze --compile --basic-header $SRC > $OUT || exit 1
x64 $OUT
rm -f $OUT
