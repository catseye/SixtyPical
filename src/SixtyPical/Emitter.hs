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

emitInstr p r (LOAD A (NamedLocation label)) = "lda " ++ label
emitInstr p r (LOAD X (NamedLocation label)) = "ldx " ++ label
emitInstr p r (LOAD Y (NamedLocation label)) = "ldy " ++ label
emitInstr p r (CMP A (NamedLocation label)) = "cmp " ++ label

emitInstr p r (COPY A X) = "tax"
emitInstr p r (COPY A Y) = "tay"
emitInstr p r (COPY X A) = "txa"
emitInstr p r (COPY Y A) = "tya"
emitInstr p r _ = "(instr)"
