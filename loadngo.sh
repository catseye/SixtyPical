#!/bin/sh

./build.sh || exit 1
bin/sixtypical emit $1 > tmp.oph || exit 1
cat lib/basic_header.oph tmp.oph > tmp2.oph || exit 1
ophis tmp2.oph -o tmp.prg || exit 1
x64 -joydev2 1 tmp.prg
rm -f tmp.oph tmp2.oph tmp.prg
