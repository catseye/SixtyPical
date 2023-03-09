#!/bin/sh

# This currently represents a lot of tests!  If you only want to run a subset,
# it's probably best to run `falderal` manually on the file(s) you want to test.
# Note also that the `sixtypical-py2.7.md` appliance, in the same directory as
# `sixtypical.md`, can be used to run the tests under Python 2.7.

falderal --substring-error \
    "tests/appliances/sixtypical.md" \
    "tests/SixtyPical Syntax.md" \
    "tests/SixtyPical Analysis.md" \
    "tests/SixtyPical Storage.md" \
    "tests/SixtyPical Control Flow.md" \
    "tests/SixtyPical Fallthru.md" \
    "tests/SixtyPical Callgraph.md" \
    "tests/SixtyPical Compilation.md"
