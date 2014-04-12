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
                 | ByteTable DataValue
    deriving (Show, Ord, Eq)

data StorageLocation = A
              | Y
              | X
              | FlagN
              | FlagV
              | FlagD
              | FlagZ
              | FlagC
              | AllFlags   -- for PHP
              | Immediate DataValue
              | Indirect StorageLocation
              | Indexed StorageLocation StorageLocation
              | IndirectIndexed StorageLocation StorageLocation
              | NamedLocation (Maybe StorageType) LocationName
              | LowByteOf StorageLocation
              | HighByteOf StorageLocation
    deriving (Show, Ord, Eq)

-- -- -- -- program model -- -- -- --

data Decl = Assign LocationName StorageType Address -- .alias
          | Reserve LocationName StorageType [DataValue] -- .word, .byte
          | External RoutineName Address
    deriving (Show, Ord, Eq)

type RoutineName = String

data Branch = BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS
    deriving (Show, Ord, Eq)

data WithInstruction = SEI
                     | PUSH StorageLocation
    deriving (Show, Ord, Eq)

data Block = Block [Decl] [Instruction]
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
                 | IF InternalID Branch Block Block
                 | REPEAT InternalID Branch Block
                 | DELTA StorageLocation DataValue
                 | WITH WithInstruction Block
                 | COPYROUTINE RoutineName StorageLocation
                 | NOP
    deriving (Show, Ord, Eq)

data Routine = Routine RoutineName [StorageLocation] Block
    deriving (Show, Ord, Eq)

data Program = Program [Decl] [Routine]
    deriving (Show, Ord, Eq)

-- -- -- accessors and helpers -- -- --

-- bit of a hack to deepseq the eval
programSummary p@(Program decls routs) =
    show ((length $ show p) < 99999)

getRoutineName (Routine name _ _) = name

getDeclLocationName (Assign name _ _) = name
getDeclLocationName (Reserve name _ _) = name

getDeclLocationType (Assign _ t _) = t
getDeclLocationType (Reserve _ t _) = t

isLocationDecl (Assign _ _ _) = True
isLocationDecl (Reserve _ _ _) = True
isLocationDecl _ = False

isInitializedDecl (Assign _ _ _) = False
isInitializedDecl (Reserve _ _ (v:vs)) = True
isInitializedDecl (Reserve _ _ []) = False

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

--

mapInstrs :: (Instruction -> Instruction) -> [Instruction] -> [Instruction]
mapInstrs = map

mapBlock :: (Instruction -> Instruction) -> Block -> Block
mapBlock f (Block decls instrs) =
    Block decls (mapInstrs f instrs)

mapRoutine :: (Instruction -> Instruction) -> Routine -> Routine
mapRoutine f (Routine name outputs block) =
    Routine name outputs (mapBlock f block)

mapRoutines :: (Instruction -> Instruction) -> [Routine] -> [Routine]
mapRoutines f [] = []
mapRoutines f (rout:routs) =
    (mapRoutine f rout):(mapRoutines f routs)

mapProgramRoutines :: (Instruction -> Instruction) -> Program -> Program
mapProgramRoutines f (Program decls routs) =
    Program decls $ mapRoutines f routs

--

foldInstrs :: (Instruction -> a -> a) -> a -> [Instruction] -> a
foldInstrs = foldr

foldBlock :: (Instruction -> a -> a) -> a -> Block -> a
foldBlock f a (Block decls instrs) =
    foldInstrs f a instrs

foldRoutine :: (Instruction -> a -> a) -> a -> Routine -> a
foldRoutine f a (Routine name outputs instrs) =
    foldBlock f a instrs

foldRoutines :: (Instruction -> a -> a) -> a -> [Routine] -> a
foldRoutines f a [] = a
foldRoutines f a (rout:routs) =
    let
        z = foldRoutine f a rout
    in
        foldRoutines f z routs

foldProgramRoutines :: (Instruction -> a -> a) -> a -> Program -> a
foldProgramRoutines f a (Program decls routs) =
    foldRoutines f a routs

foldDecls :: (Decl -> a -> a) -> a -> [Decl] -> a
foldDecls = foldr

foldProgramDecls :: (Decl -> a -> a) -> a -> Program -> a
foldProgramDecls f a (Program decls routs) =
    foldDecls f a decls

--

lookupDecl (Program decls _) name =
    lookupDecl' (filter (isLocationDecl) decls) name

lookupDecl' [] _ = Nothing
lookupDecl' (decl:decls) name
    | (getDeclLocationName decl) == name = Just decl
    | otherwise                          = lookupDecl' decls name

lookupRoutine (Program _ routines) name =
    lookupRoutine' routines name

lookupRoutine' [] _ = Nothing
lookupRoutine' (rout@(Routine rname _ _):routs) name
    | rname == name = Just rout
    | otherwise     = lookupRoutine' routs name
