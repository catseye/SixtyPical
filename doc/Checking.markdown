Checking SixtyPical Programs
============================

    -> Tests for functionality "Parse SixtyPical program"

    -> Functionality "Parse SixtyPical program" is implemented by
    -> shell command "bin/sixtypical parse %(test-file)"

    -> Tests for functionality "Check SixtyPical program"
    
    -> Functionality "Check SixtyPical program" is implemented by
    -> shell command "bin/sixtypical check %(test-file)"

`main` must be present.

    | routine main {
    |    nop
    | }
    = True

    | routine frog {
    |    nop
    | }
    ? missing 'main' routine

A comment may appear after each command.

    | routine main {
    |    lda #1   ; we assemble the fnord using
    |    ldx #1   ; multiple lorem ipsums which
    |    ldy #1
    |    lda #1   ; we
    |    ldx #1   ; found under the bridge by the old mill yesterday
    | }
    = True

A comment may appear after each declaration.

    | reserve byte lives      ; fnord
    | assign byte gdcol 647   ; fnord
    | external blastoff 4     ; fnnnnnnnnnnnnnnnnfffffffff
    | 
    | routine main {
    |   nop
    | }
    = True

A program may `reserve` and `assign`.

    | reserve byte lives
    | assign byte gdcol 647
    | reserve word score
    | assign word memstr 641
    | reserve vector v
    | assign vector cinv 788
    | reserve byte table frequencies
    | assign byte table screen 1024
    | routine main {
    |    nop
    | }
    = True

A program may declare an `external`.

    | external blastoff 49152
    | routine main {
    |    jsr blastoff
    | }
    = True

All declarations (`reserve`s and `assign`s) must come before any `routines`.

    | routine main {
    |    lda score
    | }
    | reserve word score
    ? expecting "routine"

All locations used in all routines must be declared first.

    | reserve byte score
    | routine main {
    |    lda score
    |    cmp screen
    | }
    ? undeclared location

Even in inner blocks.

    | reserve byte score
    | assign byte screen 1024
    | routine main {
    |    lda score
    |    cmp screen
    |    if beq {
    |      lda score
    |    } else {
    |      lda fnord
    |    }
    | }
    ? undeclared location

All routines jsr'ed to must be defined, or external.

    | routine main {
    |    jsr blastoff
    | }
    ? undeclared routine

No duplicate location names in declarations.

    | reserve word score
    | assign word score 4000
    | routine main {
    |    nop
    | }
    ? duplicate location name

No duplicate routine names.

    | routine main {
    |    nop
    | }
    | routine main {
    |    txa
    | }
    ? duplicate routine name

No duplicate routine names, including externals.

    | external main 7000
    | routine main {
    |    nop
    | }
    ? duplicate routine name

We can jump indirectly through a vector.

    | reserve vector blah
    | routine main {
    |    jmp (blah)
    | }
    = True

We can't jump indirectly through a word.

    | reserve word blah
    | routine main {
    |    jmp (blah)
    | }
    ? jmp to non-vector

We can't jump indirectly through a byte.

    | assign byte screen 1024
    | routine main {
    |    jmp (screen)
    | }
    ? jmp to non-vector

We can absolute-indexed a byte table.

    | assign byte table screen 1024
    | routine main {
    |    sta screen, x
    | }
    = True

We cannot absolute-indexed a byte.

    | assign byte screen 1024
    | routine main {
    |    sta screen, x
    | }
    ? indexed access of non-table

We cannot absolute-indexed a word.

    | assign word screen 1024
    | routine main {
    |    sta screen, x
    | }
    ? indexed access of non-table

We cannot absolute access a word.

    | assign word screen 1024
    | routine main {
    |    ldx screen
    | }
    ? incompatible types 'Word' and 'Byte'

No, not even with `ora`.

    | assign word screen 1024
    | routine main {
    |    ora screen
    | }
    ? incompatible types 'Byte' and 'Word'

Instead, we have to do this.

    | assign word screen 1024
    | routine main {
    |    lda <screen
    |    lda >screen
    | }
    = True

We cannot absolute access a vector.

    | assign vector screen 1024
    | routine main {
    |    lda screen
    | }
    ? incompatible types 'Vector' and 'Byte'
