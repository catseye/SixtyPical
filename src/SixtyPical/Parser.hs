-- encoding: UTF-8

module SixtyPical.Parser (parseProgram) where

import SixtyPical.Model
import Text.ParserCombinators.Parsec

{-

Toplevel := {Decl} {Routine}.
Decl     := "reserve" StorageType LocationName
          | "assign" StorageType LocationName Address.
StorageType := "byte" | "word" | "vector".
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
          | "inx" | "iny" | "dex" | "dey" | "inc" Location | "dec" Location
          | "clc" | "cld" | "clv" | "sec" | "sed"
          | "sei" Block
          | "jmp" LocationName
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
    sz <- storage_type
    name <- locationName
    return $ Reserve name sz

assign :: Parser Decl
assign = do
    string "assign"
    spaces
    sz <- storage_type
    name <- locationName
    addr <- address
    return $ Assign name sz addr

get_storage "byte" = Byte
get_storage "word" = Word
get_storage "vector" = Vector
get_storage "byte table" = ByteTable

storage_type :: Parser StorageType
storage_type = do
    s <- (try $ string "byte table") <|> (string "byte") <|>
         (string "word") <|> (string "vector")
    spaces
    return $ get_storage s

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

--command = (try lda_imm) <|> (try lda) <|>

command :: Parser Instruction
command = (try lda) <|>
          (try ldx) <|> (try ldy) <|>
          (try sta) <|> (try stx) <|> (try sty) <|>
          (try txa) <|> (try tax) <|> (try tya) <|> (try tay) <|>
          (try cmp) <|> (try cpx) <|> (try cpy) <|>
          (try inx) <|> (try iny) <|> (try dex) <|> (try dey) <|>
          (try inc) <|> (try dec) <|>
          (try clc) <|> (try cld) <|> (try clv) <|> (try sec) <|> (try sed) <|>
          (try sei) <|>
          (try jmp) <|>
          (try copy_vector_statement) <|>
          (try copy_routine_statement) <|>
          if_statement <|> repeat_statement <|> nop

nop :: Parser Instruction
nop = do
    string "nop"
    spaces
    return NOP

clc :: Parser Instruction
clc = do
    string "clc"
    spaces
    return $ PUT FlagC 0

cld :: Parser Instruction
cld = do
    string "cld"
    spaces
    return $ PUT FlagD 0

clv :: Parser Instruction
clv = do
    string "clv"
    spaces
    return $ PUT FlagV 0

sec :: Parser Instruction
sec = do
    string "sec"
    spaces
    return $ PUT FlagC 1

sed :: Parser Instruction
sed = do
    string "sed"
    spaces
    return $ PUT FlagD 1

inx :: Parser Instruction
inx = do
    string "inx"
    spaces
    return $ DELTA X 1

iny :: Parser Instruction
iny = do
    string "iny"
    spaces
    return $ DELTA Y 1

dex :: Parser Instruction
dex = do
    string "dex"
    spaces
    return $ DELTA X (-1)

dey :: Parser Instruction
dey = do
    string "dey"
    spaces
    return $ DELTA Y (-1)

inc :: Parser Instruction
inc = do
    string "inc"
    spaces
    l <- locationName
    return (DELTA (NamedLocation l) 1)

dec :: Parser Instruction
dec = do
    string "dec"
    spaces
    l <- locationName
    return (DELTA (NamedLocation l) (-1))

cmp :: Parser Instruction
cmp = do
    string "cmp"
    spaces
    (try $ immediate (\v -> CMPIMM A v) <|>
     absolute (\l -> CMP A (NamedLocation l)))

cpx :: Parser Instruction
cpx = do
    string "cpx"
    spaces
    (try $ immediate (\v -> CMPIMM X v) <|>
     absolute (\l -> CMP X (NamedLocation l)))

cpy :: Parser Instruction
cpy = do
    string "cpy"
    spaces
    (try $ immediate (\v -> CMPIMM Y v) <|>
     absolute (\l -> CMP Y (NamedLocation l)))

immediate :: (DataValue -> Instruction) -> Parser Instruction
immediate f = do
    string "#"
    v <- data_value
    return $ f v

absolute :: (LocationName -> Instruction) -> Parser Instruction
absolute f = do
    l <- locationName
    return $ f l

index :: Parser StorageLocation
index = do
    string ","
    spaces
    string "x"
    spaces
    return X

lda :: Parser Instruction
lda = do
    string "lda"
    spaces
    (try $ immediate (\v -> PUT A v) <|>
     absolute (\l -> COPY (NamedLocation l) A))

ldx :: Parser Instruction
ldx = do
    string "ldx"
    spaces
    (try $ immediate (\v -> PUT X v) <|>
     absolute (\l -> COPY (NamedLocation l) X))

ldy :: Parser Instruction
ldy = do
    string "ldy"
    spaces
    (try $ immediate (\v -> PUT Y v) <|>
     absolute (\l -> COPY (NamedLocation l) Y))

sta :: Parser Instruction
sta = do
    string "sta"
    spaces
    l <- locationName
    indexes <- many index
    return $ case indexes of
        [] ->
            COPY A (NamedLocation l)
        [X] ->
            COPYINDEXED A (NamedLocation l) X

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

sei :: Parser Instruction
sei = do
    string "sei"
    spaces
    blk <- block
    return (SEI blk)

jmp :: Parser Instruction
jmp = do
    string "jmp"
    spaces
    l <- locationName
    return $ JMPVECTOR (NamedLocation l)

if_statement :: Parser Instruction
if_statement = do
    string "if"
    spaces
    brch <- branch
    b1 <- block
    string "else"
    spaces
    b2 <- block
    return (IF 0 brch b1 b2)

repeat_statement :: Parser Instruction
repeat_statement = do
    string "repeat"
    spaces
    brch <- branch
    blk <- block
    return (REPEAT 0 brch blk)

copy_vector_statement :: Parser Instruction
copy_vector_statement = do
    string "copy"
    spaces
    string "vector"
    spaces
    src <- locationName
    string "to"
    spaces
    dst <- locationName
    return (COPYVECTOR (NamedLocation src) (NamedLocation dst))

copy_routine_statement :: Parser Instruction
copy_routine_statement = do
    string "copy"
    spaces
    string "routine"
    spaces
    src <- routineName
    string "to"
    spaces
    dst <- locationName
    return (COPYROUTINE src (NamedLocation dst))

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

data_value :: Parser DataValue
data_value = do
    digits <- many digit
    spaces
    return (read digits :: DataValue)

-- -- -- driver -- -- --

parseProgram = parse toplevel ""
