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
Command  := "beq" Block "else" Block
          | "lda" (LocationName | Immediate)
          | "ldx" (LocationName | Immediate)
          | "ldy" (LocationName | Immediate)
          | "txa" | "tax" | "tya" | "tay"
          | "cmp" (LocationName | Immediate)
          | "nop".

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
command = cmp <|> (try lda) <|> (try ldx) <|> (try ldy) <|>
          (try txa) <|> (try tax) <|> (try tya) <|> (try tay) <|>
          beq <|> nop

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
    return (CMP A l)

lda :: Parser Instruction
lda = do
    string "lda"
    spaces
    l <- locationName
    return (LOAD A l)

ldx :: Parser Instruction
ldx = do
    string "ldx"
    spaces
    l <- locationName
    return (LOAD X l)

ldy :: Parser Instruction
ldy = do
    string "ldy"
    spaces
    l <- locationName
    return (LOAD Y l)

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

beq :: Parser Instruction
beq = do
    string "beq"
    spaces
    b1 <- block
    string "else"
    spaces
    b2 <- block
    return (IFEQ b1 b2)

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
