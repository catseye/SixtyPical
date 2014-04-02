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

data StorageLocation = A
              | Y
              | X
              | FlagN
              | FlagV
              | FlagD
              | FlagZ
              | FlagC
              | NamedLocation LocationName
    deriving (Show, Ord, Eq)

-- this is bunk, man.  if a location does not appear in an analysis
-- map the meaning should be taken to be "preserved".

allRegisters = [A, X, Y, FlagN, FlagV, FlagD, FlagZ, FlagC]

-- -- -- -- program model -- -- -- --

data StorageType = Byte
                 | Word
                 | Vector
                 | ByteTable
    deriving (Show, Ord, Eq)

data Decl = Assign LocationName StorageType Address -- .alias
          | Reserve LocationName StorageType -- .word, .byte
    deriving (Show, Ord, Eq)

type RoutineName = String

data Branch = BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS
    deriving (Show, Ord, Eq)

data Instruction = PUT StorageLocation DataValue
                 | COPY StorageLocation StorageLocation
                 | COPYINDEXED StorageLocation StorageLocation StorageLocation
                 | CMPIMM StorageLocation DataValue
                 | CMP StorageLocation StorageLocation
                 | ADDIMM StorageLocation DataValue
                 | ADD StorageLocation StorageLocation
                 | ANDIMM StorageLocation DataValue
                 | AND StorageLocation StorageLocation
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
