#!/bin/sh

# This currently represents a lot of tests!  If you only want to run a subset,
# it's probably best to run `falderal` manually on the file(s) you want to test.

falderal --substring-error \
    "tests/appliances/sixtypical.md" \
    "tests/SixtyPical Syntax.md" \
    "tests/SixtyPical Analysis.md" \
    "tests/SixtyPical Storage.md" \
    "tests/SixtyPical Control Flow.md" \
    "tests/SixtyPical Fallthru.md" \
    "tests/SixtyPical Callgraph.md" \
    "tests/SixtyPical Compilation.md"
