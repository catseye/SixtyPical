-- encoding: UTF-8

module SixtyPical.Emitter where

import SixtyPical.Model

emitProgram p@(Program decls routines) =
    emitRoutines p routines ++
    emitDecls p decls

emitDecls _ [] = ""
emitDecls p (decl:decls) =
    emitDecl p decl ++ "\n" ++ emitDecls p decls

emitDecl p (Assign name _ addr) = ".alias " ++ name ++ " " ++ (show addr)
emitDecl p (Reserve name Byte) = name ++ ": .byte 0"
emitDecl p (Reserve name Word) = name ++ ": .word 0"
emitDecl p (Reserve name Vector) = name ++ ": .word 0"
emitDecl p (External name addr) = ".alias " ++ name ++ " " ++ (show addr)
emitDecl p d = error (
    "Internal error: sixtypical doesn't know how to " ++
    "emit assembler code for '" ++ (show d) ++ "'")

emitRoutines _ [] = ""
emitRoutines p (rout:routs) =
    emitRoutine p rout ++ "\n" ++ emitRoutines p routs

emitRoutine p r@(Routine name instrs) =
    name ++ ":\n" ++ emitInstrs p r instrs ++ "  rts\n"

emitInstrs _ _ [] = ""
emitInstrs p r (instr:instrs) =
    "  " ++ emitInstr p r instr ++ "\n" ++ emitInstrs p r instrs

emitInstr p r (COPY (Immediate val) A) = "lda #" ++ (show val)
emitInstr p r (COPY (Immediate val) X) = "ldx #" ++ (show val)
emitInstr p r (COPY (Immediate val) Y) = "ldy #" ++ (show val)

emitInstr p r (COPY (Immediate 0) FlagC) = "clc"
emitInstr p r (COPY (Immediate 0) FlagD) = "cld"
emitInstr p r (COPY (Immediate 0) FlagV) = "clv"
emitInstr p r (COPY (Immediate 1) FlagC) = "sec"
emitInstr p r (COPY (Immediate 1) FlagD) = "sed"

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

emitInstr p r (COPY A (Indexed (NamedLocation label) X)) = "sta " ++ label ++ ", x"
emitInstr p r (COPY A (Indexed (NamedLocation label) Y)) = "sta " ++ label ++ ", y"

emitInstr p r (COPY (Indexed (NamedLocation label) X) A) = "lda " ++ label ++ ", x"
emitInstr p r (COPY (Indexed (NamedLocation label) Y) A) = "lda " ++ label ++ ", y"

emitInstr p r (COPY A (IndirectIndexed (NamedLocation label) Y)) = "sta (" ++ label ++ "), y"

emitInstr p r (CMP A (NamedLocation label)) = "cmp " ++ label
emitInstr p r (CMP X (NamedLocation label)) = "cpx " ++ label
emitInstr p r (CMP Y (NamedLocation label)) = "cpy " ++ label

emitInstr p r (CMP A (Immediate val)) = "cmp #" ++ (show val)
emitInstr p r (CMP X (Immediate val)) = "cpx #" ++ (show val)
emitInstr p r (CMP Y (Immediate val)) = "cpy #" ++ (show val)

emitInstr p r (ADD A (NamedLocation label)) = "adc " ++ label
emitInstr p r (ADD A (Immediate val)) = "adc #" ++ (show val)

emitInstr p r (AND A (NamedLocation label)) = "and " ++ label
emitInstr p r (AND A (Immediate val)) = "and #" ++ (show val)

emitInstr p r (SUB A (NamedLocation label)) = "sbc " ++ label
emitInstr p r (SUB A (Immediate val)) = "sbc #" ++ (show val)

emitInstr p r (OR A (NamedLocation label)) = "ora " ++ label
emitInstr p r (OR A (Immediate val)) = "ora #" ++ (show val)

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

emitInstr p r (JSR routineName) =
    "jsr " ++ routineName

emitInstr p r i = error (
    "Internal error: sixtypical doesn't know how to " ++
    "emit assembler code for '" ++ (show i) ++ "'")

