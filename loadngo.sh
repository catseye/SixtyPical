#!/bin/sh

bin/sixtypical emit $1 > tmp.oph || exit 1
cat lib/basic_head.oph tmp.oph > tmp2.oph
ophis tmp2.oph -o tmp.prg || exit 1
x64 tmp.prg
rm -f tmp.oph tmp2.oph tmp.prg
