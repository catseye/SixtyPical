-- encoding: UTF-8

module SixtyPical.Parser (parseProgram) where

import Numeric (readHex)
import Data.Char (ord)

import Text.ParserCombinators.Parsec

import SixtyPical.Model

{-

Toplevel     ::= {Decl} {Routine}.
Decl         ::= "reserve" StorageType LocationName [":" InitialValue]
               | "assign" StorageType LocationName Literal
               | "external" RoutineName Address.
InitialValue ::= Literal | StringLiteral | "(" {Literal} ")".
StorageType  ::= ("byte" | "word" | "vector") ["[" Literal "]"].
Routine      ::= "routine" RoutineName ["outputs" "(" {LocationName} ")"] Block.
Block        ::= "{" {Decl} {Command} "}".
Command ::= "if" Branch Block "else" Block
          | "lda" (LocationName | Immediate)
          | "ldx" (LocationName | Immediate)
          | "ldy" (LocationName | Immediate)
          | "txa" | "tax" | "tya" | "tay"
          | "cmp" (LocationName | Immediate)
          | "cpx" (LocationName | Immediate)
          | "cpy" (LocationName | Immediate)
          | "inx" | "iny" | "dex" | "dey" | "inc" Location | "dec" Location
          | "clc" | "cld" | "clv" | "sec" | "sed"
          | "with ("sei" | "pha" | "php") Block
          | "jmp" LocationName
          | "jsr" RoutineName
          | "nop".
Branch ::= "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bvc" | "bvs".

-}

nspaces :: Parser ()
nspaces = do
    many (space <|> try block_comment <|> line_comment)
    return ()

block_comment :: Parser Char
block_comment = do
    string "/*"
    manyTill anyChar (try (string "*/"))
    return ' '

line_comment :: Parser Char
line_comment = do
    string "//"
    manyTill anyChar (char '\n')
    return ' '

toplevel :: Parser Program
toplevel = do
    nspaces
    decls <- many decl
    routines <- many routine
    return $ Program decls routines

decl :: Parser Decl
decl = try assign <|> try reserve <|> external

reserve :: Parser Decl
reserve = do
    string "reserve"
    nspaces
    sz <- storage_type
    name <- location_name
    value <- option [] (do{ string ":";
                            nspaces;
                            x <- initial_value;
                            return x })
    return $ Reserve name sz value

assign :: Parser Decl
assign = do
    string "assign"
    nspaces
    sz <- storage_type
    name <- location_name
    addr <- literal_address
    return $ Assign name sz addr

external :: Parser Decl
external = do
    string "external"
    nspaces
    name <- routineName
    addr <- literal_address
    return $ External name addr

storage :: String -> StorageType -> Parser StorageType
storage s t = do
    string s
    nspaces
    return t

table :: StorageType -> Parser StorageType
table typ = do
    string "["
    nspaces
    size <- literal_data_value
    string "]"
    nspaces
    return $ Table typ size

storage_type :: Parser StorageType
storage_type = do
    typ <- (storage "byte" Byte) <|> (storage "word" Word) <|>
           (storage "vector" Vector)
    option typ (table typ)

initial_value :: Parser [DataValue]
initial_value =
    data_value_list <|> string_literal <|> single_literal_data_value
    where
       single_literal_data_value = do
           a <- literal_data_value
           return [a]

data_value_list = do
    string "("
    nspaces
    a <- many literal_data_value
    string ")"
    nspaces
    return a

-- -- --

routine :: Parser Routine
routine = do
    string "routine"
    nspaces
    name <- routineName
    outputs <- (try routine_outputs <|> return [])
    instrs <- block
    return (Routine name outputs instrs)

routine_outputs :: Parser [StorageLocation]
routine_outputs = do
    string "outputs"
    nspaces
    string "("
    nspaces
    locations <- many location
    string ")"
    nspaces
    return locations

location = (try explicit_register <|> named_location)

block :: Parser Block
block = do
    string "{"
    nspaces
    ds <- many decl
    cs <- many command
    string "}"
    nspaces
    return (Block ds cs)

-- -- -- -- -- -- commands -- -- -- -- --

index :: Parser StorageLocation
index = do
    string ","
    nspaces
    c <- (string "x" <|> string "y")
    nspaces
    return $ case c of
        "x" -> X
        "y" -> Y

data AddressingModality = Directly LocationName
                        | HighBytely LocationName
                        | LowBytely LocationName
                        | Indirectly LocationName
                        | Immediately DataValue
                        | Implicitly StorageLocation
    deriving (Ord, Show, Eq)

low_byte_of_absolute :: Parser AddressingModality
low_byte_of_absolute = do
    string "<"
    l <- location_name
    return $ LowBytely l

high_byte_of_absolute :: Parser AddressingModality
high_byte_of_absolute = do
    string ">"
    l <- location_name
    return $ HighBytely l

indirect_location :: Parser AddressingModality
indirect_location = do
    string "("
    nspaces
    l <- location_name
    string ")"
    nspaces
    return $ Indirectly l

direct_location :: Parser AddressingModality
direct_location = do
    l <- location_name
    return $ Directly l

explicit_location :: String -> StorageLocation -> Parser StorageLocation
explicit_location s l = do
    string s
    nspaces
    return $ l

explicit_register :: Parser StorageLocation
explicit_register = ((try $ explicit_location ".a" A) <|>
                     (try $ explicit_location ".x" X) <|>
                     (explicit_location ".y" Y))

register_location :: Parser AddressingModality
register_location = do
    z <- explicit_register
    nspaces
    return $ Implicitly z  -- ironic?

immediate :: Parser AddressingModality
immediate = do
    string "#"
    v <- literal_data_value
    return $ Immediately v

addressing_mode :: String -> (AddressingModality -> [StorageLocation] -> Instruction) -> Parser Instruction
addressing_mode opcode f = do
    string opcode
    nspaces
    d <- ((try immediate) <|> (try high_byte_of_absolute) <|>
          (try low_byte_of_absolute) <|> (try indirect_location) <|>
          (try register_location) <|> (try direct_location))
    indexes <- many index
    return $ f d indexes

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
          (try asl) <|> (try bit) <|> (try eor) <|> (try lsr) <|>
          (try rol) <|> (try ror) <|>
          (try jmp) <|> (try jsr) <|>
          (try with_block) <|>
          (try copy_routine_statement) <|>
          (try copy_general_statement) <|>
          if_statement <|> repeat_statement <|> nop

nop :: Parser Instruction
nop = do
    string "nop"
    nspaces
    return NOP

asl :: Parser Instruction
asl = do
    addressing_mode "asl" gen
    where
       gen (Implicitly A) [] = SHL A (Immediate 0)
       gen (Directly l) [] = SHL (NamedLocation Nothing l) (Immediate 0)

lsr :: Parser Instruction
lsr = do
    addressing_mode "lsr" gen
    where
       gen (Implicitly A) [] = SHR A (Immediate 0)
       gen (Directly l) [] = SHR (NamedLocation Nothing l) (Immediate 0)

rol :: Parser Instruction
rol = do
    addressing_mode "rol" gen
    where
       gen (Implicitly A) [] = SHL A FlagC
       gen (Directly l) [] = SHL (NamedLocation Nothing l) FlagC

ror :: Parser Instruction
ror = do
    addressing_mode "ror" gen
    where
       gen (Implicitly A) [] = SHR A FlagC
       gen (Directly l) [] = SHR (NamedLocation Nothing l) FlagC

clc :: Parser Instruction
clc = do
    string "clc"
    nspaces
    return $ COPY (Immediate 0) FlagC

cld :: Parser Instruction
cld = do
    string "cld"
    nspaces
    return $ COPY (Immediate 0) FlagD

clv :: Parser Instruction
clv = do
    string "clv"
    nspaces
    return $ COPY (Immediate 0) FlagV

sec :: Parser Instruction
sec = do
    string "sec"
    nspaces
    return $ COPY (Immediate 1) FlagC

sed :: Parser Instruction
sed = do
    string "sed"
    nspaces
    return $ COPY (Immediate 1) FlagD

inx :: Parser Instruction
inx = do
    string "inx"
    nspaces
    return $ DELTA X 1

iny :: Parser Instruction
iny = do
    string "iny"
    nspaces
    return $ DELTA Y 1

dex :: Parser Instruction
dex = do
    string "dex"
    nspaces
    return $ DELTA X (-1)

dey :: Parser Instruction
dey = do
    string "dey"
    nspaces
    return $ DELTA Y (-1)

inc :: Parser Instruction
inc = do
    string "inc"
    nspaces
    l <- named_location
    return (DELTA l 1)

dec :: Parser Instruction
dec = do
    string "dec"
    nspaces
    l <- named_location
    return (DELTA l (-1))

cmp :: Parser Instruction
cmp = do
    addressing_mode "cmp" gen
    where
       gen (Immediately v) [] = CMP A (Immediate v)
       gen (LowBytely l) [] = CMP A (LowByteOf (NamedLocation Nothing l))
       gen (HighBytely l) [] = CMP A (HighByteOf (NamedLocation Nothing l))
       gen (Directly l) [] = CMP A (NamedLocation Nothing l)

cpx :: Parser Instruction
cpx = do
    addressing_mode "cpx" gen
    where
       gen (Immediately v) [] = CMP X (Immediate v)
       gen (Directly l) [] = CMP X (NamedLocation Nothing l)

cpy :: Parser Instruction
cpy = do
    addressing_mode "cpy" gen
    where
       gen (Immediately v) [] = CMP Y (Immediate v)
       gen (Directly l) [] = CMP Y (NamedLocation Nothing l)

adc :: Parser Instruction
adc = do
    addressing_mode "adc" gen
    where
       gen (Immediately v) [] = ADD A (Immediate v)
       gen (LowBytely l) [] = ADD A (LowByteOf (NamedLocation Nothing l))
       gen (HighBytely l) [] = ADD A (HighByteOf (NamedLocation Nothing l))
       gen (Directly l) [] = ADD A (NamedLocation Nothing l)

sbc :: Parser Instruction
sbc = do
    addressing_mode "sbc" gen
    where
       gen (Immediately v) [] = SUB A (Immediate v)
       gen (LowBytely l) [] = SUB A (LowByteOf (NamedLocation Nothing l))
       gen (HighBytely l) [] = SUB A (HighByteOf (NamedLocation Nothing l))
       gen (Directly l) [] = SUB A (NamedLocation Nothing l)

and :: Parser Instruction
and = do
    addressing_mode "and" gen
    where
       gen (Immediately v) [] = AND A (Immediate v)
       gen (Directly l) [] = AND A (NamedLocation Nothing l)

ora :: Parser Instruction
ora = do
    addressing_mode "ora" gen
    where
       gen (Immediately v) [] = OR A (Immediate v)
       gen (Directly l) [] = OR A (NamedLocation Nothing l)

eor :: Parser Instruction
eor = do
    addressing_mode "eor" gen
    where
       gen (Immediately v) [] = XOR A (Immediate v)
       gen (Directly l) [] = XOR A (NamedLocation Nothing l)

bit :: Parser Instruction
bit = do
    addressing_mode "bit" gen
    where
       gen (Directly l) [] = BIT (NamedLocation Nothing l)

lda :: Parser Instruction
lda = do
    addressing_mode "lda" gen
    where
       gen (Immediately v) [] = COPY (Immediate v) A
       gen (LowBytely l) [] = COPY (LowByteOf (NamedLocation Nothing l)) A
       gen (HighBytely l) [] = COPY (HighByteOf (NamedLocation Nothing l)) A
       gen (Directly l) [] = COPY (NamedLocation Nothing l) A
       gen (Directly l) [reg] = COPY (Indexed (NamedLocation Nothing l) reg) A
       gen (Indirectly l) [reg] = COPY (IndirectIndexed (NamedLocation Nothing l) reg) A

ldx :: Parser Instruction
ldx = do
    addressing_mode "ldx" gen
    where
       gen (Immediately v) [] = COPY (Immediate v) X
       gen (Directly l) [] = COPY (NamedLocation Nothing l) X

ldy :: Parser Instruction
ldy = do
    addressing_mode "ldy" gen
    where
       gen (Immediately v) [] = COPY (Immediate v) Y
       gen (Directly l) [] = COPY (NamedLocation Nothing l) Y

sta :: Parser Instruction
sta = do
    addressing_mode "sta" gen
    where
       gen (LowBytely l) [] = COPY A (LowByteOf (NamedLocation Nothing l))
       gen (HighBytely l) [] = COPY A (HighByteOf (NamedLocation Nothing l))
       gen (Directly l) [] = COPY A (NamedLocation Nothing l)
       gen (Directly l) [reg] = COPY A (Indexed (NamedLocation Nothing l) reg)
       gen (Indirectly l) [reg] = COPY A (IndirectIndexed (NamedLocation Nothing l) reg)

stx :: Parser Instruction
stx = do
    addressing_mode "stx" gen
    where
       gen (Directly l) [] = COPY X (NamedLocation Nothing l)
       gen (LowBytely l) [] = COPY X (LowByteOf (NamedLocation Nothing l))
       gen (HighBytely l) [] = COPY X (HighByteOf (NamedLocation Nothing l))

sty :: Parser Instruction
sty = do
    addressing_mode "sty" gen
    where
       gen (Directly l) [] = COPY Y (NamedLocation Nothing l)
       gen (LowBytely l) [] = COPY Y (LowByteOf (NamedLocation Nothing l))
       gen (HighBytely l) [] = COPY Y (HighByteOf (NamedLocation Nothing l))

txa :: Parser Instruction
txa = do
    string "txa"
    nspaces
    return (COPY X A)

tax :: Parser Instruction
tax = do
    string "tax"
    nspaces
    return (COPY A X)

tya :: Parser Instruction
tya = do
    string "tya"
    nspaces
    return (COPY Y A)

tay :: Parser Instruction
tay = do
    string "tay"
    nspaces
    return (COPY A Y)

with_block :: Parser Instruction
with_block = do
    string "with"
    nspaces
    instr <- (try sei) <|> (try pha) <|> php
    blk <- block
    return (WITH instr blk)


sei :: Parser WithInstruction
sei = do
    string "sei"
    nspaces
    return SEI

pha :: Parser WithInstruction
pha = do
    string "pha"
    nspaces
    return (PUSH A)

php :: Parser WithInstruction
php = do
    string "php"
    nspaces
    return (PUSH AllFlags)

jmp :: Parser Instruction
jmp = do
    string "jmp"
    nspaces
    string "("
    nspaces
    l <- named_location
    string ")"
    nspaces
    return $ JMPVECTOR l

jsr :: Parser Instruction
jsr = do
    string "jsr"
    nspaces
    l <- routineName
    return $ JSR l

if_statement :: Parser Instruction
if_statement = do
    string "if"
    nspaces
    brch <- branch
    b1 <- block
    string "else"
    nspaces
    b2 <- block
    return (IF 0 brch b1 b2)

repeat_statement :: Parser Instruction
repeat_statement = do
    string "repeat"
    nspaces
    brch <- branch
    blk <- block
    return (REPEAT 0 brch blk)

copy_general_statement :: Parser Instruction
copy_general_statement = do
    string "copy"
    nspaces

    src <- (try immediate <|> try direct_location)
    srcI <- many index    
    lhs <- return $ case (src, srcI) of
        ((Immediately s), []) -> (Immediate s)
        ((Directly s), []) -> (NamedLocation Nothing s)
        ((Directly s), [reg]) -> (Indexed (NamedLocation Nothing s) reg)

    dst <- direct_location
    dstI <- many index    
    rhs <- return $ case (dst, dstI) of
        ((Directly d), []) -> (NamedLocation Nothing d)
        ((Directly d), [reg]) -> (Indexed (NamedLocation Nothing d) reg)

    return $ COPY lhs rhs

copy_routine_statement :: Parser Instruction
copy_routine_statement = do
    string "copy"
    nspaces
    string "routine"
    nspaces
    src <- routineName
    string "to"
    nspaces
    dst <- location_name
    return (COPYROUTINE src (NamedLocation Nothing dst))

branch :: Parser Branch
branch = try (b "bcc" BCC) <|> try (b "bcs" BCS) <|> try (b "beq" BEQ) <|>
         try (b "bmi" BMI) <|> try (b "bne" BNE) <|> try (b "bpl" BPL) <|>
         try (b "bvc" BVC) <|> (b "bvs" BVS)

b :: String -> Branch -> Parser Branch
b s k = do
    string s
    nspaces
    return k

routineName :: Parser String
routineName = do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    nspaces
    return (c:cs)

location_name :: Parser String
location_name = do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    nspaces
    return (c:cs)

named_location :: Parser StorageLocation
named_location = do
    name <- location_name
    return (NamedLocation Nothing name)

literal_address = do
    a <- literal_value
    return (a :: Address)

literal_data_value = do
    a <- literal_value
    return (a :: DataValue)

literal_value = hex_literal <|> decimal_literal

hex_literal :: Parser Int
hex_literal = do
    char '$'
    digits <- many hexDigit
    nspaces
    let ((d, _):_) = readHex digits
    return d

decimal_literal :: Parser Int
decimal_literal = do
    digits <- many1 digit
    nspaces
    return $ read digits

string_literal :: Parser [DataValue]
string_literal = do
    char '"'
    s <- manyTill anyChar (char '"')
    nspaces
    return $ map (\c -> ord c) s

-- -- -- driver -- -- --

parseProgram = parse toplevel ""
