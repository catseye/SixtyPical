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

-- -- -- -- --

-- TODO: look at all blocks, not just routine's blocks
renameBlockDecls (Program decls routines) =
    let
        routines' = map renameRoutineDecls routines
    in
        Program decls routines'
    where
        renameRoutineDecls (Routine name outputs block) =
            let
                (Block decls _) = block
                block' = foldDeclsRenaming decls block
            in
                (Routine name outputs block')

-- TODO will have to return new decls too
-- TODO will have to take accumulator too
-- TODO accumulator has to range across all routines too!
foldDeclsRenaming [] block = block
foldDeclsRenaming ((Reserve name typ Nothing):decls) block =
    let
        newName = "_temp_1" -- TODO base this on accumulator
        block' = mapBlockNames name newName block
    in
        foldDeclsRenaming decls block'

mapBlockNames n1 n2 (Block decls instrs) =
    (Block decls $ mapInstrsNames n1 n2 instrs)

-- TODO: write this
mapInstrsNames n1 n2 instrs = instrs

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
