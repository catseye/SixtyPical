SixtyPical
==========

    -> Tests for functionality "Parse SixtyPical program"
    
    -> Functionality "Parse SixtyPical program" is implemented by
    -> shell command "bin/sixtypical parse %(test-file)"

    | routine main {
    |    nop
    | }
    = Program [] [Routine "main" [NOP]]

    | reserve word score
    | assign word scram 4000
    | routine main {
    |    lda scram
    |    cmp score
    | }
    = Program [Reserve "score" Word,Assign "scram" Word 4000] [Routine "main" [LOAD A "scram",CMP A "score"]]

All declarations (`reserve`s and `assign`s) must come before any `routines`.

    | routine main {
    |    lda scram
    | }
    | reserve word score
    ? expecting "routine"
