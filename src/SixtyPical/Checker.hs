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
                Just (Assign _ (ByteTable _) _) -> j
                Just (Reserve _ (ByteTable _) _) -> j
                Just _ -> (COPY A A)
                Nothing -> (COPY A A)
        checkInstr other = other

noUseOfUndeclaredRoutines p@(Program decls routines) =
    let
        undeclaredRoutines = foldProgramRoutines (checkInstr) 0 p
    in
        undeclaredRoutines == 0
    where
        routineNames = declaredRoutineNames p
        -- TODO also check COPYROUTINE here
        checkInstr j@(JSR routName) acc =
            case routName `elem` routineNames of
                True -> acc
                False -> error ("undeclared routine '" ++ routName ++ "'") -- acc + 1
        checkInstr other acc = acc

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
numberRoutineLoops (Routine name outputs instrs) iid =
    let
        (instrs', iid') = numberBlockLoops instrs iid
    in
        ((Routine name outputs instrs'), iid')

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
            typeMatch src dest (COPY)
        xform (CMP dest other) =
            typeMatch dest other (CMP)
        xform (ADD dest other) =
            typeMatch dest other (ADD)
        xform (AND dest other) =
            typeMatch dest other (AND)
        xform (SUB dest other) =
            typeMatch dest other (SUB)
        xform (OR dest other) =
            typeMatch dest other (OR)
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
        xform (WITH SEI blk) =
            WITH SEI (mapBlock xform blk)
        xform (WITH (PUSH val) blk) =
            WITH (PUSH (resolve val)) (mapBlock xform blk)
        xform (COPYROUTINE name dest) =
            COPYROUTINE name (resolve dest)
        xform other =
            other
        getType (NamedLocation (Just t) _) = t
        getType A = Byte
        getType X = Byte
        getType Y = Byte
        getType (Immediate x) =
            if x > 255 then Word else Byte
        getType _ = Byte
        typeMatch x y constructor =
            let
                rx = resolve x
                ry = resolve y
                typeRx = getType rx
                typeRy = getType ry
            in
                case (typeRx == typeRy, typeRx, typeRy) of
                    (True, _, _) -> constructor rx ry
                    (_, Byte, (ByteTable _)) -> constructor rx ry
                    (_, (ByteTable _), Byte) -> constructor rx ry                    
                    _ -> error ("incompatible types '" ++ (show typeRx) ++ "' and '" ++ (show typeRy) ++ "'")
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
