-- encoding: UTF-8

module SixtyPical.Model where

-- -- -- -- machine model -- -- -- --

type DataValue = Int -- LET'S ASSUME THIS IS AT LEAST 8 BITS
type Address = Int -- LET'S ASSUME THIS IS AT LEAST 16 BITS

type InternalID = Int -- for numbering labels for if/repeat

type LocationName = String

-- We do not include the PC as it of course changes constantly.
-- We do not include the stack pointer, as it should not change over
-- the lifetime of a single routine.  (Always pop what you pushed.)
-- Ditto the I flag.  (always enable interrupts after disabling them.)
-- We do not include the B flag, because for us, BRK is game over, man.

-- One of these should never refer to the program code.  We can only police
-- this up to a point.

data StorageType = Byte
                 | Word
                 | Vector
                 | ByteTable
    deriving (Show, Ord, Eq)

data StorageLocation = A
              | Y
              | X
              | FlagN
              | FlagV
              | FlagD
              | FlagZ
              | FlagC
              | Immediate DataValue
              | Indirect StorageLocation
              | Indexed StorageLocation StorageLocation
              | IndirectIndexed StorageLocation StorageLocation
              | NamedLocation (Maybe StorageType) LocationName
              | LowByteOf StorageLocation
              | HighByteOf StorageLocation
    deriving (Show, Ord, Eq)

-- this is bunk, man.  if a location does not appear in an analysis
-- map the meaning should be taken to be "preserved".

allRegisters = [A, X, Y, FlagN, FlagV, FlagD, FlagZ, FlagC]

-- -- -- -- program model -- -- -- --

data Decl = Assign LocationName StorageType Address -- .alias
          | Reserve LocationName StorageType -- .word, .byte
          | External RoutineName Address
    deriving (Show, Ord, Eq)

type RoutineName = String

data Branch = BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS
    deriving (Show, Ord, Eq)

data Instruction = COPY StorageLocation StorageLocation
                 | CMP StorageLocation StorageLocation
                 | ADD StorageLocation StorageLocation
                 | AND StorageLocation StorageLocation
                 | SUB StorageLocation StorageLocation
                 | OR StorageLocation StorageLocation
                 | XOR StorageLocation StorageLocation
                 | SHL StorageLocation StorageLocation
                 | SHR StorageLocation StorageLocation
                 | BIT StorageLocation
                 | JSR RoutineName
              -- | JSRVECTOR StorageLocation
                 | JMPVECTOR StorageLocation
                 | IF InternalID Branch [Instruction] [Instruction]
                 | REPEAT InternalID Branch [Instruction]
                 | DELTA StorageLocation DataValue
                 | SEI [Instruction]
                 | COPYVECTOR StorageLocation StorageLocation
                 | COPYROUTINE RoutineName StorageLocation
                 | NOP
    deriving (Show, Ord, Eq)

data Routine = Routine RoutineName [Instruction]
    deriving (Show, Ord, Eq)

data Program = Program [Decl] [Routine]
    deriving (Show, Ord, Eq)

-- -- -- accessors and helpers -- -- --

-- bit of a hack to deepseq the eval
programSummary p@(Program decls routs) =
    show ((length $ show p) < 99999)

getRoutineName (Routine name _) = name

getDeclLocationName (Assign name _ _) = name
getDeclLocationName (Reserve name _) = name

getDeclLocationType (Assign _ t _) = t
getDeclLocationType (Reserve _ t) = t

isLocationDecl (Assign _ _ _) = True
isLocationDecl (Reserve _ _) = True
isLocationDecl _ = False

declaredLocationNames (Program decls _) =
    map (getDeclLocationName) (filter (isLocationDecl) decls)

locationDeclared locName p =
    elem locName $ declaredLocationNames p

getDeclRoutineName (External name _) = name

isRoutineDecl (External _ _) = True
isRoutineDecl _ = False

declaredRoutineNames (Program decls routines) =
    map (getRoutineName) routines ++
      map (getDeclRoutineName) (filter (isRoutineDecl) decls)

routineDeclared routName p =
    elem routName (declaredRoutineNames p)

mapBlock :: (Instruction -> Instruction) -> [Instruction] -> [Instruction]
mapBlock = map

mapRoutine :: (Instruction -> Instruction) -> Routine -> Routine
mapRoutine f (Routine name instrs) = Routine name (mapBlock f instrs)

mapRoutines :: (Instruction -> Instruction) -> [Routine] -> [Routine]
mapRoutines f [] = []
mapRoutines f (rout:routs) =
    (mapRoutine f rout):(mapRoutines f routs)

mapProgramRoutines :: (Instruction -> Instruction) -> Program -> Program
mapProgramRoutines f (Program decls routs) = Program decls $ mapRoutines f routs

lookupDecl (Program decls _) name =
    lookupDecl' (filter (isLocationDecl) decls) name

lookupDecl' [] _ = Nothing
lookupDecl' (decl:decls) name =
    if
        (getDeclLocationName decl) == name
      then
        Just decl
      else
        lookupDecl' decls name
