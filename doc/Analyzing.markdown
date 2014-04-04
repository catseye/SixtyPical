Anayzling SixtyPical Programs
=============================

    -> Tests for functionality "Analyze SixtyPical program"
    
    -> Functionality "Analyze SixtyPical program" is implemented by
    -> shell command "bin/sixtypical analyze %(test-file)"

Analysis determines what storage locations have been modified by a
routine.

    | reserve byte score
    | routine main {
    |   lda #4
    |   sta score
    | }
    = main
    =   A: UpdatedWith (Immediate 4)
    =   NamedLocation (Just Byte) "score": UpdatedWith A

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
    ? routine does not preserve 'A'

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
    = main
    =   A: UpdatedWith (Immediate 4)
    =   X: PoisonedWith (Immediate 1)
    =   NamedLocation (Just Byte) "border_colour": UpdatedWith A
    =   NamedLocation (Just Byte) "score": PoisonedWith X
    = 
    = update_score
    =   X: UpdatedWith (Immediate 1)
    =   NamedLocation (Just Byte) "score": UpdatedWith X
