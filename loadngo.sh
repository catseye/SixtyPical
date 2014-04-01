#!/bin/sh

bin/sixtypical emit $1 > tmp.oph && ophis tmp.oph -o tmp.prg && x64 tmp.prg
rm -f tmp.oph tmp.prg
