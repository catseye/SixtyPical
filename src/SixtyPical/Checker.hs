-- encoding: UTF-8

module SixtyPical.Checker where

import SixtyPical.Model

allTrue = foldl (&&) True

trueOrDie message test =
    if test then True else error message

-- in the following, we mean Named locations

routineUsedLocations (Routine _ instrs) = blockUsedLocations instrs

blockUsedLocations [] = []
blockUsedLocations (instr:instrs) =
    (instrUsedLocations instr) ++ blockUsedLocations instrs

--instrUsedLocations (LOADIMM reg (NamedLocation loc)) = [loc]
instrUsedLocations (COPY (NamedLocation loc) _) = [loc]
instrUsedLocations (COPY _ (NamedLocation loc)) = [loc]
instrUsedLocations (CMP reg (NamedLocation loc)) = [loc]
-- TODO: JSR...
instrUsedLocations (IF _ branch b1 b2) =
    blockUsedLocations b1 ++ blockUsedLocations b2
instrUsedLocations (REPEAT _ branch blk) =
    blockUsedLocations blk
instrUsedLocations _ = []

allRoutineLocationsDeclared program routine =
    allTrue (map (isDeclared) (routineUsedLocations routine))
    where
        isDeclared name = locationDeclared name program

allUsedLocationsDeclared p@(Program _ routines) =
    allTrue (map (allRoutineLocationsDeclared p) routines)

-- --

isUnique [] = True
isUnique (x:xs) = (not (x `elem` xs)) && isUnique xs

noDuplicateDecls program =
    isUnique $ declaredLocationNames program

noDuplicateRoutines program =
    isUnique $ declaredRoutineNames program

-- wow.  efficiency is clearly our watchword
-- (and sarcasm is our backup watchword)
noJmpsToNonVectors p@(Program decls routines) =
    let
        mappedProgram = mapProgramRoutines (checkInstr) p
    in
        mappedProgram == p
    where
        checkInstr j@(JMPVECTOR (NamedLocation g)) =
            case lookupDecl p g of
                Just (Assign _ Vector _) -> j
                Just (Reserve _ Vector) -> j
                Just _ -> (COPY A A)
                Nothing -> (COPY A A)
        checkInstr other = other

noIndexedAccessOfNonTables p@(Program decls routines) =
    let
        mappedProgram = mapProgramRoutines (checkInstr) p
    in
        mappedProgram == p
    where
        checkInstr j@(COPYINDEXED _ (NamedLocation g) _) =
            case lookupDecl p g of
                Just (Assign _ ByteTable _) -> j
                Just (Reserve _ ByteTable) -> j
                Just _ -> (COPY A A)
                Nothing -> (COPY A A)
        checkInstr other = other

noUseOfUndeclaredRoutines p@(Program decls routines) =
    let
        mappedProgram = mapProgramRoutines (checkInstr) p
    in
        mappedProgram == p
    where
        routineNames = declaredRoutineNames p
        checkInstr j@(JSR routName) =
            case routName `elem` routineNames of
                True -> j
                False -> (COPY A A)
        checkInstr other = other

-- -- --

checkAndTransformProgram :: Program -> Maybe Program
checkAndTransformProgram program =
    if
        trueOrDie "missing 'main' routine" (routineDeclared "main" program) &&
        trueOrDie "undeclared location" (allUsedLocationsDeclared program) &&
        trueOrDie "duplicate location name" (noDuplicateDecls program) &&
        trueOrDie "duplicate routine name" (noDuplicateRoutines program) &&
        trueOrDie "jmp to non-vector" (noJmpsToNonVectors program) &&
        trueOrDie "undeclared routine" (noUseOfUndeclaredRoutines program) &&
        trueOrDie "indexed access of non-table" (noIndexedAccessOfNonTables program) 
      then
        Just $ numberProgramLoops program
      else Nothing

-- - - - - - -

-- in the following "number" means "assign a unique ID to" and "loop"
-- means "REPEAT or IF" (because i'm in such a good mood)

numberProgramLoops :: Program -> Program
numberProgramLoops (Program decls routines) =
    let
        (routines', _) = numberRoutinesLoops routines 0
    in
        (Program decls routines')

numberRoutinesLoops :: [Routine] -> InternalID -> ([Routine], InternalID)
numberRoutinesLoops [] iid = ([], iid)
numberRoutinesLoops (routine:routines) iid =
    let
        (routine', iid') = numberRoutineLoops routine iid
        (routines', iid'') = numberRoutinesLoops routines iid'
    in
        ((routine':routines'), iid'')

numberRoutineLoops :: Routine -> InternalID -> (Routine, InternalID)
numberRoutineLoops (Routine name instrs) iid =
    let
        (instrs', iid') = numberBlockLoops instrs iid
    in
        ((Routine name instrs'), iid')

numberBlockLoops :: [Instruction] -> InternalID -> ([Instruction], InternalID)
numberBlockLoops [] iid = ([], iid)
numberBlockLoops (instr:instrs) iid =
    let
        (instr', iid') = numberInstruction instr iid
        (instrs', iid'') = numberBlockLoops instrs iid'
    in
        ((instr':instrs'), iid'')

numberInstruction :: Instruction -> InternalID -> (Instruction, InternalID)
numberInstruction (IF _ branch b1 b2) iid =
    let
        (b1', iid') = numberBlockLoops b1 iid
        (b2', iid'') = numberBlockLoops b2 iid'
        newIid = iid'' + 1
        newInstr = IF newIid branch b1' b2'
    in
        (newInstr, newIid)
numberInstruction (REPEAT _ branch blk) iid =
    let
        (blk', iid') = numberBlockLoops blk iid
        newIid = iid' + 1
        newInstr = REPEAT newIid branch blk'
    in
        (newInstr, newIid)
numberInstruction i iid = (i, iid)
