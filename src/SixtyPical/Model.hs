-- encoding: UTF-8

module SixtyPical.Model where

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
