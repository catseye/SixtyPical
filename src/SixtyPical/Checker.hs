-- encoding: UTF-8

module SixtyPical.Checker where

import SixtyPical.Model

allTrue = foldl (&&) True

trueOrDie message test =
    if test then True else error message

isUnique [] = True
isUnique (x:xs) = (not (x `elem` xs)) && isUnique xs

-- --

noDuplicateDecls program =
    isUnique $ declaredLocationNames program

noDuplicateRoutines program =
    isUnique $ declaredRoutineNames program

-- wow.  efficiency is clearly our watchword
-- (and sarcasm is our backup watchword)
noIndexedAccessOfNonTables p@(Program decls routines) =
    let
        mappedProgram = mapProgramRoutines (checkInstr) p
    in
        mappedProgram == p
    where
        checkInstr j@(COPY _ (Indexed (NamedLocation sz g) reg)) =
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
        -- TODO also check COPYROUTINE here
        checkInstr j@(JSR routName) =
            case routName `elem` routineNames of
                True -> j
                False -> (COPY A A)
        checkInstr other = other

-- -- -- -- -- --

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

-- -- --

fillOutNamedLocationTypes p@(Program decls routines) =
    mapProgramRoutines (xform) p
    where
        xform (COPY src dest) =
            COPY (resolve src) (resolve dest)
        xform (CMP dest other) =
            CMP (resolve dest) (resolve other)
        xform (ADD dest other) =
            ADD (resolve dest) (resolve other)
        xform (AND dest other) =
            AND (resolve dest) (resolve other)
        xform (SUB dest other) =
            SUB (resolve dest) (resolve other)
        xform (OR dest other) =
            OR (resolve dest) (resolve other)
        xform (JMPVECTOR dest) =
            case (resolve dest) of
                d@(NamedLocation (Just Vector) _) ->
                    JMPVECTOR d
                _ ->
                    error ("jmp to non-vector '" ++ (show dest) ++ "'")
        xform (IF iid branch b1 b2) =
            IF iid branch (mapBlock xform b1) (mapBlock xform b2)
        xform (REPEAT iid branch blk) =
            REPEAT iid branch (mapBlock xform blk)
        xform (DELTA dest val) =
            DELTA (resolve dest) val
        xform (SEI blk) =
            SEI (mapBlock xform blk)
        xform (COPYVECTOR src dest) =
            COPYVECTOR (resolve src) (resolve dest)
        xform (COPYROUTINE name dest) =
            COPYROUTINE name (resolve dest)
        xform other =
            other
        resolve (NamedLocation Nothing name) =
            case lookupDecl p name of
                Just decl ->
                    (NamedLocation (Just $ getDeclLocationType decl) name)
                _ ->
                    error ("undeclared location '" ++ name ++ "'")
        resolve (Indirect loc) =
            (Indirect (resolve loc))
        resolve (Indexed loc reg) =
            (Indexed (resolve loc) (resolve reg))
        resolve (IndirectIndexed loc reg) =
            (IndirectIndexed (resolve loc) (resolve reg))
        resolve other =
            other

-- - - - - - -

checkAndTransformProgram :: Program -> Maybe Program
checkAndTransformProgram program =
    if
        trueOrDie "missing 'main' routine" (routineDeclared "main" program) &&
        trueOrDie "duplicate location name" (noDuplicateDecls program) &&
        trueOrDie "duplicate routine name" (noDuplicateRoutines program) &&
        trueOrDie "undeclared routine" (noUseOfUndeclaredRoutines program) &&
        trueOrDie "indexed access of non-table" (noIndexedAccessOfNonTables program) 
      then
        let
            program' = numberProgramLoops program
            program'' = fillOutNamedLocationTypes program'
        in
            Just program''
      else Nothing
