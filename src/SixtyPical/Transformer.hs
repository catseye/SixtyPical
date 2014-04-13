-- encoding: UTF-8

module SixtyPical.Transformer (
    numberProgramLoops, fillOutNamedLocationTypes,
    renameBlockDecls, liftBlockDecls
  ) where

import SixtyPical.Model

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
numberRoutineLoops (Routine name outputs block) iid =
    let
        (block', iid') = numberBlockLoops block iid
    in
        ((Routine name outputs block'), iid')

numberBlockLoops :: Block -> InternalID -> (Block, InternalID)
numberBlockLoops block iid =
    let
        (Block decls instrs) = block
        (instrs', iid') = numberInstrsLoops instrs iid
        block' = Block decls instrs'
    in
        (block', iid')

numberInstrsLoops :: [Instruction] -> InternalID -> ([Instruction], InternalID)
numberInstrsLoops [] iid = ([], iid)
numberInstrsLoops (instr:instrs) iid =
    let
        (instr', iid') = numberInstruction instr iid
        (instrs', iid'') = numberInstrsLoops instrs iid'
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
        getType (Immediate x) =   -- TODO! allow promotion!
            if x > 255 then Word else Byte
        getType (Indexed t _) =
            getType t
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
                    (_, Byte, (Table Byte _)) -> constructor rx ry
                    (_, (Table Byte _), Byte) -> constructor rx ry                    
                    (_, Word, (Table Word _)) -> constructor rx ry
                    (_, (Table Word _), Word) -> constructor rx ry                    
                    _ -> error ("incompatible types '" ++ (show typeRx) ++ "' and '" ++ (show typeRy) ++ "'" ++
                                "  " ++ (show rx) ++ "," ++ (show ry))
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
        resolve (LowByteOf loc) =
            (LowByteOf (resolve loc))
        resolve (HighByteOf loc) =
            (HighByteOf (resolve loc))
        resolve other =
            other

-- -- -- -- --

-- TODO: look at all blocks, not just routine's blocks
renameBlockDecls (Program decls routines) =
    let
        routines' = renameRoutineDecls 1 routines
    in
        Program decls routines'

renameRoutineDecls id [] = []
renameRoutineDecls id ((Routine name outputs block):routs) =
    let
        (Block decls _) = block
        (id', block') = foldDeclsRenaming decls id block
        rest = renameRoutineDecls id' routs
    in
        ((Routine name outputs block'):rest)

foldDeclsRenaming [] id block = (id, block)
foldDeclsRenaming ((Reserve name typ []):decls) id block =
    let
        newName = "_temp_" ++ (show id)
        id' = id + 1
        block' = mapBlockNames name newName block
        block'' = substDeclName name newName block'
    in
        foldDeclsRenaming decls id' block''
foldDeclsRenaming ((Reserve name typ _):decls) id block =
    error ("block-level '" ++ name ++ "' cannot supply initial value")

-- this is kind of horrible.  that we do it this way, i mean
substDeclName n1 n2 (Block decls instrs) =
    Block (map (s) decls) instrs
    where
        s d@(Reserve name typ [])
          | name == n1   = (Reserve n2 typ [])
          | otherwise    = d


mapBlockNames n1 n2 (Block decls instrs) =
    (Block decls $ mapInstrsNames n1 n2 instrs)

mapInstrsNames n1 n2 instrs =
    map (mapInstrName n1 n2) instrs

mapInstrName n1 n2 (COPY sl1 sl2) =
   COPY (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (CMP sl1 sl2) =
   CMP (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (ADD sl1 sl2) =
   ADD (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (AND sl1 sl2) =
   AND (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (SUB sl1 sl2) =
   SUB (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (OR sl1 sl2) =
   OR (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (XOR sl1 sl2) =
   XOR (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (SHL sl1 sl2) =
   SHL (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (SHR sl1 sl2) =
   SHR (mapStorageLocationName n1 n2 sl1) (mapStorageLocationName n1 n2 sl2)
mapInstrName n1 n2 (BIT sl1) =
   BIT (mapStorageLocationName n1 n2 sl1)
mapInstrName n1 n2 (JMPVECTOR sl1) =
   JMPVECTOR (mapStorageLocationName n1 n2 sl1)
mapInstrName n1 n2 (DELTA sl1 v) =
   DELTA (mapStorageLocationName n1 n2 sl1) v

mapInstrName n1 n2 (IF id branch b1 b2) =
   IF id branch (mapBlockNames n1 n2 b1) (mapBlockNames n1 n2 b2)

mapInstrName n1 n2 (REPEAT id branch b1) =
   REPEAT id branch (mapBlockNames n1 n2 b1)

mapInstrName n1 n2 (WITH instr b1) =
   WITH instr (mapBlockNames n1 n2 b1)

{-
                 | COPYROUTINE RoutineName StorageLocation
-}

mapInstrName n1 n2 other =
    other

mapStorageLocationName n1 n2 (Indirect sl) =
    Indirect $ mapStorageLocationName n1 n2 sl
mapStorageLocationName n1 n2 (Indexed sl1 sl2) =
    Indexed (mapStorageLocationName n1 n2 sl1) sl2
mapStorageLocationName n1 n2 (IndirectIndexed sl1 sl2) =
    IndirectIndexed (mapStorageLocationName n1 n2 sl1) sl2

mapStorageLocationName n1 n2 sl@(NamedLocation typ name)
    | name == n1   = NamedLocation typ n2
    | otherwise    = sl

mapStorageLocationName n1 n2 (LowByteOf sl) =
    LowByteOf $ mapStorageLocationName n1 n2 sl

mapStorageLocationName n1 n2 (HighByteOf sl) =
    HighByteOf $ mapStorageLocationName n1 n2 sl

mapStorageLocationName n1 n2 other =
    other

-- -- -- --

-- TODO: look at all blocks, not just routine's blocks
liftBlockDecls (Program decls routines) =
    let
        liftedDecls = foldr getRoutinesBlockDecls [] routines
    in
        Program (decls ++ liftedDecls) routines
    where
        getRoutinesBlockDecls (Routine name outputs block) a =
            a ++ (getBlockDecls block)
        getBlockDecls (Block decls instrs) =
            decls
