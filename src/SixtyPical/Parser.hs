-- encoding: UTF-8

module SixtyPical.Parser (parseProgram) where

import Numeric (readHex)

import Text.ParserCombinators.Parsec

import SixtyPical.Model

{-

Toplevel := {Decl [Comment]} {Routine}.
Decl     := "reserve" StorageType LocationName
          | "assign" StorageType LocationName Address
          | "external" RoutineName Address.
StorageType := "byte" | "word" | "vector".
Routine  := "routine" RoutineName Block.
Block    := "{" [Comment] {Command [Comment]} "}".
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
          | "jsr" RoutineName
          | "nop".
Branch   := "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bvc" | "bvs".

-}

toplevel :: Parser Program
toplevel = do
    decls <- many decl
    routines <- many routine
    return $ Program decls routines

decl :: Parser Decl
decl = do
    d <- (try assign <|> try reserve <|> try external)
    optional comment
    return d

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

external :: Parser Decl
external = do
    string "external"
    spaces
    name <- routineName
    addr <- address
    return $ External name addr

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
    optional comment
    cs <- many commented_command
    string "}"
    spaces
    return cs

comment :: Parser ()
comment = do
    string ";"
    manyTill anyChar (try (string "\n"))
    spaces
    return ()

-- -- -- -- -- -- commands -- -- -- -- --

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
    c <- (string "x" <|> string "y")
    spaces
    return $ case c of
        "x" -> X
        "y" -> Y

data Directness = Directly LocationName
                | Indirectly LocationName
    deriving (Ord, Show, Eq)

indirect_location :: Parser Directness
indirect_location = do
    string "("
    spaces
    l <- locationName
    string ")"
    spaces
    return $ Indirectly l

direct_location :: Parser Directness
direct_location = do
    l <- locationName
    return $ Directly l

directness_location = (try indirect_location) <|> direct_location

indirect_indexed :: (Directness -> [StorageLocation] -> Instruction) -> Parser Instruction
indirect_indexed f = do
    d <- directness_location
    indexes <- many index
    return $ f d indexes

absolute_indexed :: (LocationName -> [StorageLocation] -> Instruction) -> Parser Instruction
absolute_indexed f = do
    l <- locationName
    indexes <- many index
    return $ f l indexes

commented_command :: Parser Instruction
commented_command = do
    c <- command
    optional comment
    return c

command :: Parser Instruction
command = (try lda) <|>
          (try ldx) <|> (try ldy) <|>
          (try sta) <|> (try stx) <|> (try sty) <|>
          (try txa) <|> (try tax) <|> (try tya) <|> (try tay) <|>
          (try cmp) <|> (try cpx) <|> (try cpy) <|>
          (try inx) <|> (try iny) <|> (try dex) <|> (try dey) <|>
          (try inc) <|> (try dec) <|>
          (try clc) <|> (try cld) <|> (try clv) <|> (try sec) <|> (try sed) <|>
          (try adc) <|> (try SixtyPical.Parser.and) <|>
          (try sbc) <|> (try ora) <|>
          (try sei) <|>
          (try jmp) <|> (try jsr) <|>
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
    return $ COPY (Immediate 0) FlagC

cld :: Parser Instruction
cld = do
    string "cld"
    spaces
    return $ COPY (Immediate 0) FlagD

clv :: Parser Instruction
clv = do
    string "clv"
    spaces
    return $ COPY (Immediate 0) FlagV

sec :: Parser Instruction
sec = do
    string "sec"
    spaces
    return $ COPY (Immediate 1) FlagC

sed :: Parser Instruction
sed = do
    string "sed"
    spaces
    return $ COPY (Immediate 1) FlagD

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

adc :: Parser Instruction
adc = do
    string "adc"
    spaces
    (try $ immediate (\v -> ADDIMM A v) <|>
     absolute (\l -> ADD A (NamedLocation l)))

sbc :: Parser Instruction
sbc = do
    string "sbc"
    spaces
    (try $ immediate (\v -> SUBIMM A v) <|>
     absolute (\l -> SUB A (NamedLocation l)))

and :: Parser Instruction
and = do
    string "and"
    spaces
    (try $ immediate (\v -> ANDIMM A v) <|>
     absolute (\l -> AND A (NamedLocation l)))

ora :: Parser Instruction
ora = do
    string "ora"
    spaces
    (try $ immediate (\v -> ORIMM A v) <|>
     absolute (\l -> OR A (NamedLocation l)))

lda :: Parser Instruction
lda = do
    string "lda"
    spaces
    (try $ immediate (\v -> COPY (Immediate v) A) <|> absolute_indexed gen)
    where
       gen l [] = COPY (NamedLocation l) A
       gen l [reg] = COPY (Indexed (NamedLocation l) reg) A

ldx :: Parser Instruction
ldx = do
    string "ldx"
    spaces
    (try $ immediate (\v -> COPY (Immediate v) X) <|>
     absolute (\l -> COPY (NamedLocation l) X))

ldy :: Parser Instruction
ldy = do
    string "ldy"
    spaces
    (try $ immediate (\v -> COPY (Immediate v) Y) <|>
     absolute (\l -> COPY (NamedLocation l) Y))

sta :: Parser Instruction
sta = do
    string "sta"
    spaces
    indirect_indexed gen
    where
       gen (Directly l) [] = COPY A (NamedLocation l)
       gen (Directly l) [reg] = COPY A (Indexed (NamedLocation l) reg)
       gen (Indirectly l) [reg] = COPY A (IndirectIndexed (NamedLocation l) reg)

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

jsr :: Parser Instruction
jsr = do
    string "jsr"
    spaces
    l <- routineName
    return $ JSR l

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

address = hex_address <|> decimal_address

hex_address :: Parser Address
hex_address = do
    char '$'
    digits <- many hexDigit
    spaces
    let ((d, _):_) = readHex digits
    return (d :: Address)

decimal_address :: Parser Address
decimal_address = do
    digits <- many digit
    spaces
    return (read digits :: Address)

data_value = hex_data_value <|> decimal_data_value

hex_data_value :: Parser DataValue
hex_data_value = do
    char '$'
    digits <- many hexDigit
    spaces
    let ((d, _):_) = readHex digits
    return (d :: DataValue)

decimal_data_value :: Parser DataValue
decimal_data_value = do
    digits <- many digit
    spaces
    return (read digits :: DataValue)

-- -- -- driver -- -- --

parseProgram = parse toplevel ""
