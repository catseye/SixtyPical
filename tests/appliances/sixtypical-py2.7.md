This file contains only the [Falderal][] directives that define the different
functionalities tested by the test suite, assuming that it's the reference
implementation, `sixtypical`, that is going to implement these functionalities,
and additionally that `sixtypical` is running under Python 2.7.

NOTE that this is not well-supported anymore, given that Python 2.7 is past
end-of-life.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Check syntax of SixtyPical program" is implemented by
    -> shell command "python2.7 bin/sixtypical --parse-only --traceback %(test-body-file) && echo ok"

    -> Functionality "Analyze SixtyPical program" is implemented by
    -> shell command "python2.7 bin/sixtypical --analyze-only --traceback %(test-body-file) && echo ok"

    -> Functionality "Compile SixtyPical program" is implemented by
    -> shell command "python2.7 bin/sixtypical --output-format=c64-basic-prg --traceback %(test-body-file) --output /tmp/foo && python2.7 tests/appliances/bin/dcc6502-adapter </tmp/foo"

    -> Functionality "Dump callgraph info for SixtyPical program" is implemented by
    -> shell command "python2.7 bin/sixtypical --dump-callgraph --analyze-only --traceback %(test-body-file)"

    -> Functionality "Compile SixtyPical program with unreachable routine removal" is implemented by
    -> shell command "python2.7 bin/sixtypical --output-format=c64-basic-prg --prune-unreachable-routines --traceback %(test-body-file) --output /tmp/foo && python2.7 tests/appliances/bin/dcc6502-adapter </tmp/foo"

    -> Functionality "Dump fallthru info for SixtyPical program" is implemented by
    -> shell command "python2.7 bin/sixtypical --optimize-fallthru --dump-fallthru-info --analyze-only --traceback %(test-body-file)"

    -> Functionality "Compile SixtyPical program with fallthru optimization" is implemented by
    -> shell command "python2.7 bin/sixtypical --output-format=c64-basic-prg --optimize-fallthru --traceback %(test-body-file) --output /tmp/foo && python2.7 tests/appliances/bin/dcc6502-adapter </tmp/foo"
