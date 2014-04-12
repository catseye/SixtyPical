-- encoding: UTF-8

module SixtyPical.Emitter where

import Data.Bits

import SixtyPical.Model

emitProgram p@(Program decls routines) =
    let
        mains = filter (\(Routine name _ _) -> name == "main") routines
        allElse = filter (\(Routine name _ _) -> name /= "main") routines
        initializedDecls = filter (\d -> isInitializedDecl d) decls
        uninitializedDecls = filter (\d -> not $ isInitializedDecl d) decls
    in
        emitRoutines p mains ++
        emitRoutines p allElse ++
        emitDecls p initializedDecls ++
        (case uninitializedDecls of
            [] -> ""
            _  -> ".data\n" ++ emitDecls p uninitializedDecls)

emitDecls _ [] = ""
emitDecls p (decl:decls) =
    emitDecl p decl ++ "\n" ++ emitDecls p decls

emitDecl p (Assign name _ addr) = ".alias " ++ name ++ " " ++ (show addr)
emitDecl p (Reserve name typ [val])
    | typ == Byte = name ++ ": .byte " ++ (show val)
    | typ == Word = name ++ ": .word " ++ (show val)
    | typ == Vector = name ++ ": .word " ++ (show val)

emitDecl p (Reserve name (Table Byte size) []) =
    ".space " ++ name ++ " " ++ (show size)

emitDecl p (Reserve name (Table Byte size) vals) =
    name ++ ": .byte " ++ (showList vals)
    where
        showList [] = ""
        showList [val] = show val
        showList (val:vals) = (show val) ++ ", " ++ (showList vals)

emitDecl p (Reserve name typ [])
    | typ == Byte = ".space " ++ name ++ " 1"
    | typ == Word = ".space " ++ name ++ " 2"
    | typ == Vector = ".space " ++ name ++ " 2"

emitDecl p (External name addr) = ".alias " ++ name ++ " " ++ (show addr)
emitDecl p d = error (
    "Internal error: sixtypical doesn't know how to " ++
    "emit assembler code for '" ++ (show d) ++ "'")

emitRoutines _ [] = ""
emitRoutines p (rout:routs) =
    emitRoutine p rout ++ "\n" ++ emitRoutines p routs

emitRoutine p r@(Routine name _ block) =
    name ++ ":\n" ++ emitBlock p r block ++ "  rts\n"

emitBlock p r (Block decls instrs) =
    emitInstrs p r instrs

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

emitInstr p r (COPY A (Indexed (NamedLocation (Just (Table Byte _)) label) X)) = "sta " ++ label ++ ", x"
emitInstr p r (COPY A (Indexed (NamedLocation (Just (Table Byte _)) label) Y)) = "sta " ++ label ++ ", y"

emitInstr p r (COPY (Indexed (NamedLocation (Just (Table Byte _)) label) X) A) = "lda " ++ label ++ ", x"
emitInstr p r (COPY (Indexed (NamedLocation (Just (Table Byte _)) label) Y) A) = "lda " ++ label ++ ", y"

emitInstr p r (COPY A (IndirectIndexed (NamedLocation st label) Y)) = "sta (" ++ label ++ "), y"
emitInstr p r (COPY (IndirectIndexed (NamedLocation st label) Y) A) = "lda (" ++ label ++ "), y"

emitInstr p r (COPY (NamedLocation (Just st1) src) (NamedLocation (Just st2) dst))
  | (st1 == Vector && st2 == Vector) || (st1 == Word && st2 == Word) =
    "lda " ++ src ++ "\n" ++
    "  sta " ++ dst ++ "\n" ++
    "  lda " ++ src ++ "+1\n" ++
    "  sta " ++ dst ++ "+1"

emitInstr p r (COPY (Immediate v) (NamedLocation (Just st) dst))
  | st == Byte =
    "lda #" ++ (show v) ++ "\n" ++
    "  sta " ++ dst
  | st == Word =
    let
        low = v .&. 255
        high = (shift v (-8)) .&. 255
    in
        "lda #" ++ (show low) ++ "\n" ++
        "  sta " ++ dst ++ "\n" ++
        "  lda #" ++ (show high) ++ "\n" ++
        "  sta " ++ dst ++ "+1"

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

emitInstr p r (XOR A (NamedLocation st label)) = "eor " ++ label
emitInstr p r (XOR A (Immediate val)) = "eor #" ++ (show val)

emitInstr p r (SHL A (Immediate 0)) = "asl"
emitInstr p r (SHL (NamedLocation st label) (Immediate 0)) = "asl " ++ label
emitInstr p r (SHR A (Immediate 0)) = "lsr"
emitInstr p r (SHR (NamedLocation st label) (Immediate 0)) = "lsr " ++ label
emitInstr p r (SHL A FlagC) = "rol"
emitInstr p r (SHL (NamedLocation st label) FlagC) = "rol " ++ label
emitInstr p r (SHR A FlagC) = "ror"
emitInstr p r (SHR (NamedLocation st label) FlagC) = "ror " ++ label

emitInstr p r (BIT (NamedLocation st label)) = "bit " ++ label

emitInstr p r (DELTA X 1) = "inx"
emitInstr p r (DELTA X (-1)) = "dex"
emitInstr p r (DELTA Y 1) = "iny"
emitInstr p r (DELTA Y (-1)) = "dey"
emitInstr p r (DELTA (NamedLocation st label) 1) = "inc " ++ label
emitInstr p r (DELTA (NamedLocation st label) (-1)) = "dec " ++ label

emitInstr p r (IF iid branch b1 b2) =
    (show branch) ++ " _label_" ++ (show iid) ++ "\n" ++
    emitBlock p r b2 ++
    "  jmp _past_" ++ (show iid) ++ "\n" ++
    "_label_" ++ (show iid) ++ ":\n" ++
    emitBlock p r b1 ++
    "_past_" ++ (show iid) ++ ":"

emitInstr p r (REPEAT iid branch blk) =
    "\n_repeat_" ++ (show iid) ++ ":\n" ++
    emitBlock p r blk ++
    "  " ++ (show branch) ++ " _repeat_" ++ (show iid)

emitInstr p r (WITH SEI blk) =
    "sei\n" ++
    emitBlock p r blk ++
    "  cli"

emitInstr p r (WITH (PUSH A) blk) =
    "pha\n" ++
    emitBlock p r blk ++
    "  pla"

emitInstr p r (WITH (PUSH AllFlags) blk) =
    "php\n" ++
    emitBlock p r blk ++
    "  plp"

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

