-- encoding: UTF-8

module SixtyPical.Emitter where

import SixtyPical.Model

basicHeader =
    ".org 0\n" ++
    ".word $0801\n" ++
    ".org $0801\n" ++
    ".byte $10, $08, $c9, $07, $9e, $32, $30, $36, $31, $00, $00, $00\n" ++
    "  jmp main\n"

emitProgram p@(Program decls routines) =
    basicHeader ++
    emitDecls p decls ++
    emitRoutines p routines

emitDecls _ [] = ""
emitDecls p (decl:decls) =
    emitDecl p decl ++ "\n" ++ emitDecls p decls

emitDecl p (Assign name _ addr) = ".alias " ++ name ++ " " ++ (show addr)
emitDecl p (Reserve name Byte) = name ++ ": .byte 0"
emitDecl p (Reserve name Word) = name ++ ": .word 0"
emitDecl p (Reserve name Vector) = name ++ ": .word 0"

emitRoutines _ [] = ""
emitRoutines p (rout:routs) =
    emitRoutine p rout ++ "\n" ++ emitRoutines p routs

emitRoutine p r@(Routine name instrs) =
    name ++ ":\n" ++ emitInstrs p r instrs ++ "  rts\n"

emitInstrs _ _ [] = ""
emitInstrs p r (instr:instrs) =
    "  " ++ emitInstr p r instr ++ "\n" ++ emitInstrs p r instrs

emitInstr p r (PUT A val) = "lda #" ++ (show val)
emitInstr p r (PUT X val) = "ldx #" ++ (show val)
emitInstr p r (PUT Y val) = "ldy #" ++ (show val)

emitInstr p r (PUT FlagC 0) = "clc"
emitInstr p r (PUT FlagD 0) = "cld"
emitInstr p r (PUT FlagV 0) = "clv"
emitInstr p r (PUT FlagC 1) = "sec"
emitInstr p r (PUT FlagD 1) = "sed"

emitInstr p r (COPY A (NamedLocation label)) = "sta " ++ label
emitInstr p r (COPY X (NamedLocation label)) = "stx " ++ label
emitInstr p r (COPY Y (NamedLocation label)) = "sty " ++ label
emitInstr p r (COPY (NamedLocation label) A) = "lda " ++ label
emitInstr p r (COPY (NamedLocation label) X) = "ldx " ++ label
emitInstr p r (COPY (NamedLocation label) Y) = "ldy " ++ label

emitInstr p r (COPY A X) = "tax"
emitInstr p r (COPY A Y) = "tay"
emitInstr p r (COPY X A) = "txa"
emitInstr p r (COPY Y A) = "tya"

emitInstr p r (COPYINDEXED A (NamedLocation label) X) = "sta " ++ label ++ ", x"
emitInstr p r (COPYINDEXED A (NamedLocation label) Y) = "sta " ++ label ++ ", x"

emitInstr p r (COPYINDEXED (NamedLocation label) A X) = "lda " ++ label ++ ", x"
emitInstr p r (COPYINDEXED (NamedLocation label) A Y) = "lda " ++ label ++ ", x"

emitInstr p r (CMP A (NamedLocation label)) = "cmp " ++ label
emitInstr p r (CMP X (NamedLocation label)) = "cpx " ++ label
emitInstr p r (CMP Y (NamedLocation label)) = "cpy " ++ label

emitInstr p r (CMPIMM A val) = "cmp #" ++ (show val)
emitInstr p r (CMPIMM X val) = "cpx #" ++ (show val)
emitInstr p r (CMPIMM Y val) = "cpy #" ++ (show val)

emitInstr p r (DELTA X 1) = "inx"
emitInstr p r (DELTA X (-1)) = "dex"
emitInstr p r (DELTA Y 1) = "iny"
emitInstr p r (DELTA Y (-1)) = "dey"
emitInstr p r (DELTA (NamedLocation label) 1) = "inc " ++ label
emitInstr p r (DELTA (NamedLocation label) (-1)) = "dec " ++ label

emitInstr p r (IF iid branch b1 b2) =
    (show branch) ++ " _label_" ++ (show iid) ++ "\n" ++
    emitInstrs p r b2 ++
    "  jmp _past_" ++ (show iid) ++ "\n" ++
    "_label_" ++ (show iid) ++ ":\n" ++
    emitInstrs p r b1 ++
    "_past_" ++ (show iid) ++ ":"

emitInstr p r (REPEAT iid branch blk) =
    "\n_repeat_" ++ (show iid) ++ ":\n" ++
    emitInstrs p r blk ++
    "  " ++ (show branch) ++ " _repeat_" ++ (show iid)

emitInstr p r (SEI blk) =
    "sei\n" ++
    emitInstrs p r blk ++
    "  cli"

emitInstr p r (COPYVECTOR (NamedLocation src) (NamedLocation dst)) =
    "lda " ++ src ++ "\n" ++
    "  sta " ++ dst ++ "\n" ++
    "  lda " ++ src ++ "+1\n" ++
    "  sta " ++ dst ++ "+1"

emitInstr p r (COPYROUTINE src (NamedLocation dst)) =
    "lda #<" ++ src ++ "\n" ++
    "  sta " ++ dst ++ "\n" ++
    "  lda #>" ++ src ++ "\n" ++
    "  sta " ++ dst ++ "+1"

emitInstr p r (JMPVECTOR (NamedLocation dst)) =
    "jmp (" ++ dst ++ ")"

emitInstr p r i = error (
    "Internal error: sixtypical doesn't know how to " ++
    "emit assembler code for '" ++ (show i) ++ "'")

