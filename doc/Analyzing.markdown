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
    =   NamedLocation Nothing "score": UpdatedWith A

A routine cannot expect registers which a called routine does not
preserve, to be preserved.  We say the called routine "poisons" those
registers.

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
    ? routine 'main' does not preserve 'A'

But if a called routine does preserve those registers, the caller can
continue to use them after calling the routine.

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
    =   NamedLocation Nothing "border_colour": UpdatedWith A
    =   NamedLocation Nothing "score": PoisonedWith X
    = 
    = update_score ([])
    =   X: UpdatedWith (Immediate 1)
    =   NamedLocation Nothing "score": UpdatedWith X

Not only registers, but also named variables, can be poisoned by a called
routine.

    | reserve byte score
    | routine update_score
    | {
    |   lda #8
    |   sta score
    | }
    | routine main {
    |   jsr update_score
    |   lda score
    | }
    ? routine 'main' does not preserve 'NamedLocation Nothing "score"'

Of course, the difference between poisoning and intentionally modifying a
storage location is a matter of intent.  The solution to the above is to
explicitly notate `update_score` as an "output" of the routine.

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
    =   X: UpdatedWith (NamedLocation Nothing "score")
    =   NamedLocation Nothing "score": UpdatedWith A
    = 
    = update_score ([NamedLocation Nothing "score"])
    =   A: UpdatedWith (Immediate 8)
    =   NamedLocation Nothing "score": UpdatedWith A

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
    ? routine 'main' does not preserve 'A'

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
    =   NamedLocation Nothing "score": UpdatedWith A
    = 
    = update_score ([A])
    =   A: UpdatedWith (Immediate 8)

If a location is poisoned in either branch of an `if`, it is poisoned
after the `if`.  Note there are several tests for this.

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
    ? routine 'main' does not preserve 'A'

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
    ? routine 'main' does not preserve 'A'

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
    ? routine 'main' does not preserve 'A'

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
    ? routine 'main' does not preserve 'A'

    | reserve byte score
    | routine update_score
    | {
    |   ldx #4
    |   stx score
    | }
    | routine main {
    |   lda #4
    |   if beq {
    |      jsr update_score
    |   } else {
    |      ldx #4
    |   }
    |   sta score
    | }
    = main ([])
    =   A: UpdatedWith (Immediate 4)
    =   X: PoisonedWith (Immediate 4)
    =   NamedLocation Nothing "score": UpdatedWith A
    = 
    = update_score ([])
    =   X: UpdatedWith (Immediate 4)
    =   NamedLocation Nothing "score": UpdatedWith X

    | assign word position $fb
    | reserve byte value
    | 
    | routine reset_position {
    |     lda #$00
    |     sta <position
    |     lda #$04
    |     sta >position
    | }
    | 
    | routine main {
    |     inc value
    |     lda value
    |     ldy #0
    |     sta (position), y
    |     if beq {
    |         jsr reset_position
    |     } else {
    |     }
    | }
    = main ([])
    =   A: PoisonedWith (Immediate 4)
    =   Y: UpdatedWith (Immediate 0)
    =   IndirectIndexed (NamedLocation Nothing "position") Y: UpdatedWith A
    =   NamedLocation Nothing "position": PoisonedWith A
    =   NamedLocation Nothing "value": UpdatedWith (Immediate 1)
    = 
    = reset_position ([])
    =   A: UpdatedWith (Immediate 4)
    =   NamedLocation Nothing "position": UpdatedWith A

    | assign word position $fb
    | reserve byte value
    | 
    | routine reset_position {
    |     lda #$00
    |     sta <position
    |     lda #$04
    |     sta >position
    | }
    | 
    | routine main {
    |     inc value
    |     lda value
    |     ldy #0
    |     sta (position), y
    |     if beq {
    |         jsr reset_position
    |     } else {
    |     }
    |     sta value
    | }
    ? routine 'main' does not preserve 'A'

    | assign word position $fb
    | reserve byte value
    | 
    | routine reset_position {
    |     lda #$00
    |     sta <position
    |     lda #$04
    |     sta >position
    | }
    | 
    | routine main {
    |     inc value
    |     lda value
    |     ldy #0
    |     sta (position), y
    |     jsr reset_position
    |     if beq {
    |     } else {
    |         sta value
    |     }
    | }
    ? routine 'main' does not preserve 'A'

A storage location poisoned in a `repeat` continues to be poisoned
after the `repeat`.

    | reserve byte value
    | 
    | routine blah {
    |     lda #123
    | }
    | routine main {
    |     lda #33
    |     ldy #255
    |     repeat bne {
    |         jsr blah
    |         dey
    |     }
    |     sta value
    | }
    ? routine 'main' does not preserve 'A'

Oh, here's a tricky one.  The accumulator isn't poisoned on the first run
through the `repeat`, but it **is** on the second run through.  We handle
this simply by abstractly interpreting the `repeat`'s block twice — the
second time in the context of having already interpreted it once.

    | reserve byte value
    | 
    | routine blah {
    |     lda #123
    | }
    | routine main {
    |     lda #33
    |     ldy #255
    |     repeat bne {
    |         sta value
    |         jsr blah
    |         dey
    |     }
    | }
    ? routine 'main' does not preserve 'A'

Poisoning a high byte or low byte of a word poisons the whole word.

    | reserve word score
    | reserve byte temp
    | routine update_score
    | {
    |   ldx #4
    |   stx <score
    | }
    | routine main {
    |   jsr update_score
    |   lda >score
    |   sta temp
    | }
    ? routine 'main' does not preserve 'NamedLocation Nothing "score"'

    | reserve word score
    | reserve byte temp
    | routine update_score
    | {
    |   ldx #4
    |   stx >score
    | }
    | routine main {
    |   jsr update_score
    |   lda <score
    |   sta temp
    | }
    ? routine 'main' does not preserve 'NamedLocation Nothing "score"'
