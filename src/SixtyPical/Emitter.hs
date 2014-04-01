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

emitRoutines _ [] = ""
emitRoutines p (rout:routs) =
    emitRoutine p rout ++ "\n" ++ emitRoutines p routs

emitRoutine p r@(Routine name instrs) =
    name ++ ":\n" ++ emitInstrs p r instrs ++ "  rts\n"

emitInstrs _ _ [] = ""
emitInstrs p r (instr:instrs) =
    "  " ++ emitInstr p r instr ++ "\n" ++ emitInstrs p r instrs

emitInstr p r (LOADIMM A val) = "lda #" ++ (show val)
emitInstr p r (LOADIMM X val) = "ldx #" ++ (show val)
emitInstr p r (LOADIMM Y val) = "ldy #" ++ (show val)

emitInstr p r (COPY A (NamedLocation label)) = "sta " ++ label
emitInstr p r (COPY X (NamedLocation label)) = "stx " ++ label
emitInstr p r (COPY Y (NamedLocation label)) = "sty " ++ label
emitInstr p r (COPY (NamedLocation label) A) = "lda " ++ label
emitInstr p r (COPY (NamedLocation label) X) = "ldx " ++ label
emitInstr p r (COPY (NamedLocation label) Y) = "ldy " ++ label

emitInstr p r (CMP A (NamedLocation label)) = "cmp " ++ label
emitInstr p r (CMP X (NamedLocation label)) = "cpx " ++ label
emitInstr p r (CMP Y (NamedLocation label)) = "cpy " ++ label

emitInstr p r (DELTA X 1) = "inx"
emitInstr p r (DELTA X (-1)) = "dex"
emitInstr p r (DELTA Y 1) = "iny"
emitInstr p r (DELTA Y (-1)) = "dey"
emitInstr p r (DELTA (NamedLocation label) 1) = "inc " ++ label
emitInstr p r (DELTA (NamedLocation label) (-1)) = "dec " ++ label

emitInstr p r (COPY A X) = "tax"
emitInstr p r (COPY A Y) = "tay"
emitInstr p r (COPY X A) = "txa"
emitInstr p r (COPY Y A) = "tya"

emitInstr p r (IF branch b1 b2) =
    (show branch) ++ " _label\n" ++
    emitInstrs p r b2 ++
    "  jmp _past\n" ++
    "_label:\n" ++
    emitInstrs p r b1 ++
    "_past:"

emitInstr p r (REPEAT branch blk) =
    "\n_repeat:\n" ++
    emitInstrs p r blk ++
    "  " ++ (show branch) ++ " _repeat"

emitInstr p r i = error "Internal error: sixtypical doesn't know how to emit assembler code for '" ++ show i ++ "'"

