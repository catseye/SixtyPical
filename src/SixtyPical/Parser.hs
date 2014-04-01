-- encoding: UTF-8

module SixtyPical.Parser (parseProgram) where

import SixtyPical.Model
import Text.ParserCombinators.Parsec

{-

Toplevel := {Decl} {Routine}.
Decl     := "reserve" Size LocationName
          | "assign" Size LocationName Address.
Size     := "byte" | "word".
Routine  := "routine" RoutineName Block.
Block    := "{" {Command} "}".
Command  := "if" Branch Block "else" Block
          | "lda" (LocationName | Immediate)
          | "ldx" (LocationName | Immediate)
          | "ldy" (LocationName | Immediate)
          | "txa" | "tax" | "tya" | "tay"
          | "cmp" (LocationName | Immediate)
          | "cpx" (LocationName | Immediate)
          | "cpy" (LocationName | Immediate)
          | "nop".
Branch   := "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bvc" | "bvs".

-}

toplevel :: Parser Program
toplevel = do
    decls <- many (assign <|> try reserve)
    routines <- many routine
    return $ Program decls routines

reserve :: Parser Decl
reserve = do
    string "reserve"
    spaces
    sz <- size
    spaces  -- size does not do its own spacesising
    name <- locationName
    return $ Reserve name sz

assign :: Parser Decl
assign = do
    string "assign"
    spaces
    sz <- size
    spaces  -- size does not do its own spacesising
    name <- locationName
    addr <- address
    return $ Assign name sz addr

size :: Parser Size
size = do
    s <- (string "byte") <|> (string "word")
    return $ case s of
        "byte" -> Byte
        "word" -> Word

routine :: Parser Routine
routine = do
    string "routine"
    spaces
    name <- routineName
    instrs <- block
    return (Routine name instrs)

block :: Parser [Instruction]
block = do
    string "{"
    spaces
    cs <- many command
    string "}"
    spaces
    return cs

command :: Parser Instruction
command = (try lda) <|> (try ldx) <|> (try ldy) <|>
          (try sta) <|> (try stx) <|> (try sty) <|>
          (try txa) <|> (try tax) <|> (try tya) <|> (try tay) <|>
          (try cmp) <|> (try cpx) <|> (try cpy) <|>
          if_statement <|> nop

nop :: Parser Instruction
nop = do
    string "nop"
    spaces
    return NOP

cmp :: Parser Instruction
cmp = do
    string "cmp"
    spaces
    l <- locationName
    return (CMP A (NamedLocation l))

cpx :: Parser Instruction
cpx = do
    string "cpx"
    spaces
    l <- locationName
    return (CMP X (NamedLocation l))

cpy :: Parser Instruction
cpy = do
    string "cpy"
    spaces
    l <- locationName
    return (CMP Y (NamedLocation l))

lda :: Parser Instruction
lda = do
    string "lda"
    spaces
    l <- locationName
    return (COPY (NamedLocation l) A)

ldx :: Parser Instruction
ldx = do
    string "ldx"
    spaces
    l <- locationName
    return (COPY (NamedLocation l) X)

ldy :: Parser Instruction
ldy = do
    string "ldy"
    spaces
    l <- locationName
    return (COPY (NamedLocation l) Y)

sta :: Parser Instruction
sta = do
    string "sta"
    spaces
    l <- locationName
    return (COPY A (NamedLocation l))

stx :: Parser Instruction
stx = do
    string "stx"
    spaces
    l <- locationName
    return (COPY X (NamedLocation l))

sty :: Parser Instruction
sty = do
    string "sty"
    spaces
    l <- locationName
    return (COPY Y (NamedLocation l))

txa :: Parser Instruction
txa = do
    string "txa"
    spaces
    return (COPY X A)

tax :: Parser Instruction
tax = do
    string "tax"
    spaces
    return (COPY A X)

tya :: Parser Instruction
tya = do
    string "tya"
    spaces
    return (COPY Y A)

tay :: Parser Instruction
tay = do
    string "tay"
    spaces
    return (COPY A Y)

if_statement :: Parser Instruction
if_statement = do
    string "if"
    spaces
    brch <- branch
    b1 <- block
    string "else"
    spaces
    b2 <- block
    return (IF brch b1 b2)

branch :: Parser Branch
branch = try (b "bcc" BCC) <|> try (b "bcs" BCS) <|> try (b "beq" BEQ) <|>
         try (b "bmi" BMI) <|> try (b "bne" BNE) <|> try (b "bpl" BPL) <|>
         try (b "bvc" BVC) <|> (b "bvs" BVS)

b :: String -> Branch -> Parser Branch
b s k = do
    string s
    spaces
    return k

routineName :: Parser String
routineName = do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    spaces
    return (c:cs)

locationName :: Parser String
locationName = do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    spaces
    return (c:cs)

address :: Parser Address
address = do
    digits <- many digit
    spaces
    return (read digits :: Address)

-- -- -- driver -- -- --

parseProgram = parse toplevel ""
