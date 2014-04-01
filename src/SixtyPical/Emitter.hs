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

emitInstr p r (COPY A X) = "tax"
emitInstr p r (COPY A Y) = "tay"
emitInstr p r (COPY X A) = "txa"
emitInstr p r (COPY Y A) = "tya"
emitInstr p r _ = "(instr)"
