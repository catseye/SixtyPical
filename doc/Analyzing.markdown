Anayzling SixtyPical Programs
=============================

    -> Tests for functionality "Analyze SixtyPical program"
    
    -> Functionality "Analyze SixtyPical program" is implemented by
    -> shell command "bin/sixtypical analyze %(test-file)"

A routine cannot expect registers which a called routine does not
preserve, to be preserved.

    | assign byte border_colour 4000
    | reserve byte score
    | routine update_score
    | {
    |   lda #8
    |   sta score
    | }
    | routine main {
    |   lda #4
    |   jsr update_score
    |   sta border_colour
    | }
    ? routine does not preserve register

But if it does it can.

    | assign byte border_colour 4000
    | reserve byte score
    | routine update_score
    | {
    |   ldx score
    |   inx
    |   stx score
    | }
    | routine main {
    |   lda #4
    |   jsr update_score
    |   sta border_colour
    | }
    = True
