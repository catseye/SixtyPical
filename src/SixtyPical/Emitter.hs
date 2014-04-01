-- encoding: UTF-8

module SixtyPical.Emitter where

import SixtyPical.Model

emitProgram p@(Program decls routines) =
    ".org $c000\n" ++
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

emitInstr p r (LOAD A label) = "lda " ++ label
emitInstr p r (LOAD X label) = "ldx " ++ label
emitInstr p r (LOAD Y label) = "ldy " ++ label
emitInstr p r (CMP A label) = "cmp " ++ label

emitInstr p r (COPY A X) = "tax"
emitInstr p r (COPY A Y) = "tay"
emitInstr p r (COPY X A) = "txa"
emitInstr p r (COPY Y A) = "tya"
emitInstr p r _ = "(instr)"
