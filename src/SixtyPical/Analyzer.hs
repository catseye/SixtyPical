-- encoding: UTF-8

module SixtyPical.Analyzer where

import qualified Data.Map as Map

import SixtyPical.Model

-- -- -- -- data-flow-analysis context -- -- -- --

data Usage = Unknown
           | Value DataValue -- obviously a bit daft for now
           | Retained StorageLocation
    deriving (Show, Ord, Eq)

type RoutineContext = Map.Map StorageLocation Usage

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

analyzeProgram (Program decls routines) =
    checkRoutines routines Map.empty

checkRoutines [] progCtx = progCtx
checkRoutines (rout@(Routine name _) : routs) progCtx =
    let
        routCtx = Map.fromList $ map (\reg -> (reg, Retained reg)) allRegisters
        routAnalysis = checkRoutine rout progCtx routCtx
        progCtx' = Map.insert name routAnalysis progCtx
    in
        checkRoutines routs progCtx'

-- TODO: have this call checkblock on its instrs, use checkblock below too...
checkRoutine (Routine _ []) progCtx routCtx = routCtx
checkRoutine (Routine name (instr : instrs)) progCtx routCtx =
    let
        routCtx' = checkInstr instr progCtx routCtx
    in
        checkRoutine (Routine name instrs) progCtx routCtx'

checkInstr (LOADIMM reg imm) progCtx routCtx =
    Map.insert reg (Value imm) routCtx
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
checkInstr (IF _ branch b1 b2) progCtx routCtx =
    -- TODO: oooh, this one's gonna be fun
    routCtx
checkInstr (REPEAT _ branch blk) progCtx routCtx =
    -- TODO: oooh, this one's gonna be fun too
    routCtx
checkInstr NOP progCtx routCtx =
    routCtx
