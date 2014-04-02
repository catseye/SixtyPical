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

emitInstr p r (COPY A (NamedLocation st label)) = "sta " ++ label
emitInstr p r (COPY X (NamedLocation st label)) = "stx " ++ label
emitInstr p r (COPY Y (NamedLocation st label)) = "sty " ++ label
emitInstr p r (COPY (NamedLocation st label) A) = "lda " ++ label
emitInstr p r (COPY (NamedLocation st label) X) = "ldx " ++ label
emitInstr p r (COPY (NamedLocation st label) Y) = "ldy " ++ label

emitInstr p r (COPY (LowByteOf (NamedLocation st label)) A) = "lda " ++ label
emitInstr p r (COPY (HighByteOf (NamedLocation st label)) A) = "lda " ++ label ++ "+1"

emitInstr p r (COPY A (LowByteOf (NamedLocation st label))) = "sta " ++ label
emitInstr p r (COPY A (HighByteOf (NamedLocation st label))) = "sta " ++ label ++ "+1"

emitInstr p r (COPY A X) = "tax"
emitInstr p r (COPY A Y) = "tay"
emitInstr p r (COPY X A) = "txa"
emitInstr p r (COPY Y A) = "tya"

emitInstr p r (COPY A (Indexed (NamedLocation (Just ByteTable) label) X)) = "sta " ++ label ++ ", x"
emitInstr p r (COPY A (Indexed (NamedLocation (Just ByteTable) label) Y)) = "sta " ++ label ++ ", y"

emitInstr p r (COPY (Indexed (NamedLocation (Just ByteTable) label) X) A) = "lda " ++ label ++ ", x"
emitInstr p r (COPY (Indexed (NamedLocation (Just ByteTable) label) Y) A) = "lda " ++ label ++ ", y"

emitInstr p r (COPY A (IndirectIndexed (NamedLocation st label) Y)) = "sta (" ++ label ++ "), y"
emitInstr p r (COPY (IndirectIndexed (NamedLocation st label) Y) A) = "lda (" ++ label ++ "), y"

emitInstr p r (CMP A (NamedLocation st label)) = "cmp " ++ label
emitInstr p r (CMP X (NamedLocation st label)) = "cpx " ++ label
emitInstr p r (CMP Y (NamedLocation st label)) = "cpy " ++ label

emitInstr p r (CMP A (Immediate val)) = "cmp #" ++ (show val)
emitInstr p r (CMP X (Immediate val)) = "cpx #" ++ (show val)
emitInstr p r (CMP Y (Immediate val)) = "cpy #" ++ (show val)

emitInstr p r (CMP A (LowByteOf (NamedLocation st label))) = "cmp " ++ label
emitInstr p r (CMP A (HighByteOf (NamedLocation st label))) = "cmp " ++ label ++ "+1"

emitInstr p r (ADD A (NamedLocation st label)) = "adc " ++ label
emitInstr p r (ADD A (Immediate val)) = "adc #" ++ (show val)

emitInstr p r (ADD A (LowByteOf (NamedLocation st label))) = "adc " ++ label
emitInstr p r (ADD A (HighByteOf (NamedLocation st label))) = "adc " ++ label ++ "+1"

emitInstr p r (AND A (NamedLocation st label)) = "and " ++ label
emitInstr p r (AND A (Immediate val)) = "and #" ++ (show val)

emitInstr p r (SUB A (NamedLocation st label)) = "sbc " ++ label
emitInstr p r (SUB A (Immediate val)) = "sbc #" ++ (show val)

emitInstr p r (OR A (NamedLocation st label)) = "ora " ++ label
emitInstr p r (OR A (Immediate val)) = "ora #" ++ (show val)

emitInstr p r (DELTA X 1) = "inx"
emitInstr p r (DELTA X (-1)) = "dex"
emitInstr p r (DELTA Y 1) = "iny"
emitInstr p r (DELTA Y (-1)) = "dey"
emitInstr p r (DELTA (NamedLocation st label) 1) = "inc " ++ label
emitInstr p r (DELTA (NamedLocation st label) (-1)) = "dec " ++ label

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

emitInstr p r (COPYVECTOR (NamedLocation (Just Vector) src) (NamedLocation (Just Vector) dst)) =
    "lda " ++ src ++ "\n" ++
    "  sta " ++ dst ++ "\n" ++
    "  lda " ++ src ++ "+1\n" ++
    "  sta " ++ dst ++ "+1"

emitInstr p r (COPYROUTINE src (NamedLocation (Just Vector) dst)) =
    "lda #<" ++ src ++ "\n" ++
    "  sta " ++ dst ++ "\n" ++
    "  lda #>" ++ src ++ "\n" ++
    "  sta " ++ dst ++ "+1"

emitInstr p r (JMPVECTOR (NamedLocation (Just Vector) dst)) =
    "jmp (" ++ dst ++ ")"

emitInstr p r (JSR routineName) =
    "jsr " ++ routineName

emitInstr p r i = error (
    "Internal error: sixtypical doesn't know how to " ++
    "emit assembler code for '" ++ (show i) ++ "'")

