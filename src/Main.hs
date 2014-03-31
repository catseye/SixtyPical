-- encoding: UTF-8

module Main where

import qualified Data.Map as Map

import System.IO
import System.Environment
import System.Exit

import Text.ParserCombinators.Parsec

-- -- -- -- machine model -- -- -- --

type Address = Int -- LET'S ASSUME THIS IS AT LEAST 16 BITS

type LocationName = String

data Register = A | X | Y    -- | MemLoc LocationName
    deriving (Show, Ord, Eq)

allRegisters = [A, X, Y]

-- -- -- -- program model -- -- -- --

data Size = Byte
          | Word
    deriving (Show, Ord, Eq)

data Decl = Assign LocationName Size Address -- .alias
          | Reserve LocationName Size -- .word, .byte
    deriving (Show, Ord, Eq)

type RoutineName = String

data Instruction = LOAD Register LocationName
                 | COPY Register Register
                 | CMP Register LocationName
                 | JSR RoutineName
                 | IFEQ [Instruction] [Instruction]
                 | NOP
    deriving (Show, Ord, Eq)

data Routine = Routine RoutineName [Instruction]
    deriving (Show, Ord, Eq)

data Program = Program [Decl] [Routine]
    deriving (Show, Ord, Eq)

-- -- -- -- data-flow-analysis context -- -- -- --

data Usage = Unknown
           | Value LocationName -- obviously a bit daft for now
           | Retained Register
    deriving (Show, Ord, Eq)

type RoutineContext = Map.Map Register Usage

type ProgramContext = Map.Map RoutineName RoutineContext

--
-- Utility function:
-- Take 2 routine contexts -- the current routine and a routine that was just
-- JSR'ed to (immediately previously) -- and merge them to create a new
-- context for the current routine.
--
mergeRoutCtxs routCtx calledRoutCtx =
    let
        -- insert the values into routCtx
        -- TODO, first compare them
        -- TODO, if not equal, 'poison' them
        -- TODO, other special cases (eg Unknown)
        poison key value routCtxAccum =
            case value of
                -- if the called routine retains it,
                -- we keep our idea of it -- but TODO
                -- should we mark it "was retained"?
                Retained reg ->
                    routCtxAccum
                _ ->
                    Map.insert key value routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx calledRoutCtx

-- -- -- -- static analyzer -- -- -- --

checkProgram (Program decls routines) =
    checkRoutines routines Map.empty

checkRoutines [] progCtx = progCtx
checkRoutines (rout@(Routine name _) : routs) progCtx =
    let
        routCtx = Map.fromList $ map (\reg -> (reg, Retained reg)) allRegisters
        routAnalysis = checkRoutine rout progCtx routCtx
        progCtx' = Map.insert name routAnalysis progCtx
    in
        checkRoutines routs progCtx'

checkRoutine (Routine _ []) progCtx routCtx = routCtx
checkRoutine (Routine name (instr : instrs)) progCtx routCtx =
    let
        routCtx' = checkInstr instr progCtx routCtx
    in
        checkRoutine (Routine name instrs) progCtx routCtx'

checkInstr (LOAD reg addr) progCtx routCtx =
    Map.insert reg (Value addr) routCtx
checkInstr (COPY src dst) progCtx routCtx =
    Map.insert dst (Map.findWithDefault Unknown src routCtx) routCtx
checkInstr (JSR name) progCtx routCtx =
    case Map.lookup name progCtx of
        Just calledRoutCtx ->
            mergeRoutCtxs routCtx calledRoutCtx
        Nothing ->
            error ("can't call routine '" ++ name ++ "' before it is defined")
checkInstr (CMP reg addr) progCtx routCtx =
    -- TODO: mark Carry bit as "touched" here
    routCtx
checkInstr (IFEQ b1 b2) progCtx routCtx =
    -- TODO: oooh, this one's gonna be fun
    routCtx
checkInstr NOP progCtx routCtx =
    routCtx

-- -- -- -- parser -- -- -- --
{-

Toplevel := {Decl} {Routine}.
Decl     := "reserve" Size LocationName
          | "assign" Size LocationName Address.
Size     := "byte" | "word".
Routine  := "routine" RoutineName Block.
Block    := "{" {Command} "}".
Command  := "beq" Block "else" Block
          | "lda" (LocationName | Immediate)
          | "txa" | "tax" | "tya" | "tay"
          | "cmp" (LocationName | Immediate)

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
command = cmp <|> lda <|> beq <|> nop

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

-- -- -- -- driver -- -- -- --

usage = do
    putStrLn "Usage: sixtypical (parse|check) filename.60pical"
    exitWith $ ExitFailure 1

main = do
    args <- getArgs
    case args of
        [verb, filename] -> do
            programText <- readFile filename
            case (verb, parse toplevel "" programText) of
                ("parse", Right program) -> do
                    putStrLn $ show $ program
                ("check", Right program) -> do
                    putStrLn $ show $ checkProgram program
                (_, Left problem) -> do
                    hPutStrLn stderr (show problem)
                    exitWith $ ExitFailure 1
                (_, _) -> usage
        _ -> usage
