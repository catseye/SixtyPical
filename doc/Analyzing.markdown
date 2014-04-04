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
    = main ([])
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
    = main ([])
    =   A: UpdatedWith (Immediate 4)
    =   X: PoisonedWith (Immediate 1)
    =   NamedLocation (Just Byte) "border_colour": UpdatedWith A
    =   NamedLocation (Just Byte) "score": PoisonedWith X
    = 
    = update_score ([])
    =   X: UpdatedWith (Immediate 1)
    =   NamedLocation (Just Byte) "score": UpdatedWith X

We can't expect to stay named variables to stay unmodified either.

    | assign byte border_colour 4000
    | reserve byte score
    | routine update_score
    | {
    |   lda #8
    |   sta score
    | }
    | routine main {
    |   jsr update_score
    |   ldx score
    | }
    ? routine does not preserve 'NamedLocation (Just Byte) "score"'

What the solution to the above is to notate `update_score` as intentionally
modifying score, as an "output" of the routine.

    | assign byte border_colour 4000
    | reserve byte score
    | routine update_score outputs (score)
    | {
    |   lda #8
    |   sta score
    | }
    | routine main {
    |   ldx score
    |   jsr update_score
    |   ldx score
    | }
    = main ([])
    =   A: PoisonedWith (Immediate 8)
    =   X: UpdatedWith (NamedLocation (Just Byte) "score")
    =   NamedLocation (Just Byte) "score": UpdatedWith A
    = 
    = update_score ([NamedLocation Nothing "score"])
    =   A: UpdatedWith (Immediate 8)
    =   NamedLocation (Just Byte) "score": UpdatedWith A

Routines can name registers as outputs.

    | reserve byte score
    | routine update_score
    | {
    |   lda #8
    | }
    | routine main {
    |   jsr update_score
    |   sta score
    | }
    ? routine does not preserve 'A'

    | reserve byte score
    | routine update_score outputs (.a)
    | {
    |   lda #8
    | }
    | routine main {
    |   jsr update_score
    |   sta score
    | }
    = main ([])
    =   A: UpdatedWith (Immediate 8)
    =   NamedLocation (Just Byte) "score": UpdatedWith A
    = 
    = update_score ([A])
    =   A: UpdatedWith (Immediate 8)

If a location is poisoned in either branch of an `if`, it is poisoned
after the `if`.

    | reserve byte score
    | routine update_score
    | {
    |   if beq {
    |      lda #8
    |   } else {
    |      ldx #8
    |   }
    | }
    | routine main {
    |   lda #4
    |   jsr update_score
    |   sta score
    | }
    ? routine does not preserve 'A'

    | reserve byte score
    | routine update_score
    | {
    |   if beq {
    |      ldx #8
    |   } else {
    |      lda #8
    |   }
    | }
    | routine main {
    |   lda #4
    |   jsr update_score
    |   sta score
    | }
    ? routine does not preserve 'A'

    | reserve byte score
    | routine update_score
    | {
    |   lda #4
    |   sta score
    | }
    | routine main {
    |   lda #4
    |   if beq {
    |      jsr update_score
    |   } else {
    |      ldx #3
    |   }
    |   sta score
    | }
    ? routine does not preserve 'A'

    | reserve byte score
    | routine update_score
    | {
    |   lda #4
    |   sta score
    | }
    | routine main {
    |   lda #4
    |   if beq {
    |      ldx #3
    |   } else {
    |      jsr update_score
    |   }
    |   sta score
    | }
    ? routine does not preserve 'A'
