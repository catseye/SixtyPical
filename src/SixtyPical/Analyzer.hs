-- encoding: UTF-8

module SixtyPical.Analyzer where

import qualified Data.Map as Map

import SixtyPical.Model
import SixtyPical.Context

-- -- -- -- abstract interpreter -- -- -- --

analyzeProgram program@(Program decls routines) =
    checkRoutines routines Map.empty
    where
      checkRoutines [] progCtx = progCtx
      checkRoutines (rout@(Routine name outputs _) : routs) progCtx =
          let
              routCtx = Map.empty
              routAnalysis = checkRoutine rout progCtx routCtx
              progCtx' = Map.insert name routAnalysis progCtx
          in
              checkRoutines routs progCtx'
      
      checkRoutine (Routine name outputs instrs) progCtx routCtx =
          checkBlock instrs progCtx routCtx
      
      checkBlock [] progCtx routCtx = routCtx
      checkBlock (instr:instrs) progCtx routCtx =
          let
              routCtx' = checkInstr instr progCtx routCtx
          in
              checkBlock instrs progCtx routCtx'
      
      -- -- -- -- -- -- -- -- -- -- -- --
      
      checkInstr (COPY src dst) progCtx routCtx =
          updateRoutCtx dst (UpdatedWith src) routCtx
      checkInstr (DELTA dst val) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith (Immediate val)) routCtx

      checkInstr (ADD dst src) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith src) routCtx
      checkInstr (SUB dst src) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith src) routCtx

      checkInstr (AND dst src) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith src) routCtx
      checkInstr (OR dst src) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith src) routCtx
      checkInstr (XOR dst src) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith src) routCtx

      checkInstr (JSR name) progCtx routCtx =
          let
              Just calledRout = lookupRoutine program name
          in
              case Map.lookup name progCtx of
                  Just calledRoutCtx ->
                      mergeRoutCtxs routCtx calledRoutCtx calledRout
                  Nothing ->
                      error ("can't call routine '" ++ name ++ "' before it is defined")
      checkInstr (CMP reg addr) progCtx routCtx =
          -- TODO: mark Carry bit as "touched" here
          routCtx
      checkInstr (IF _ branch b1 b2) progCtx routCtx =
          let
              routCtx1 = checkBlock b1 progCtx routCtx
              routCtx2 = checkBlock b2 progCtx routCtx
          in
              mergeAlternateRoutCtxs routCtx1 routCtx2
      checkInstr (REPEAT _ branch blk) progCtx routCtx =
          -- TODO: oooh, this one's gonna be fun too
          --checkBlock blk progCtx routCtx
          routCtx

      -- TODO -- THESE ARE WEAK --
      checkInstr (SEI blk) progCtx routCtx =
          checkBlock blk progCtx routCtx
      checkInstr (PUSH _ blk) progCtx routCtx =
          checkBlock blk progCtx routCtx

      checkInstr (BIT dst) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith (Immediate 0)) routCtx

      checkInstr (SHR dst flg) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith flg) routCtx
      checkInstr (SHL dst flg) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith flg) routCtx

      checkInstr (COPYROUTINE name dst) progCtx routCtx =
          -- TODO check that dst is not poisoned
          updateRoutCtx dst (UpdatedWith (Immediate 7)) routCtx

      checkInstr (JMPVECTOR dst) progCtx routCtx =
          routCtx

      checkInstr NOP progCtx routCtx =
          routCtx

      checkInstr instr _ _ = error (
          "Internal error: sixtypical doesn't know how to " ++
          "analyze '" ++ (show instr) ++ "'")

--
-- Utility function:
-- Take 2 routine contexts -- the current routine and a routine that was just
-- JSR'ed to (immediately previously) -- and merge them to create a new
-- context for the current routine.
--
mergeRoutCtxs routCtx calledRoutCtx calledRout@(Routine name outputs _) =
    let
        -- go through all the Usages in the calledRoutCtx
        -- insert any that were updated, into routCtx
        poison location usage routCtxAccum =
            case usage of
                UpdatedWith ulocation ->
                    case location `elem` outputs of
                        True ->
                            updateRoutCtx location usage routCtxAccum
                        False ->
                            updateRoutCtx location (PoisonedWith ulocation) routCtxAccum
                PoisonedWith ulocation ->
                    updateRoutCtx location usage routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx calledRoutCtx

--
-- Utility function:
-- Take 2 routine contexts -- one from each branch of an `if` -- and merge
-- them to create a new context for the remainder of the routine.
--
mergeAlternateRoutCtxs routCtx1 routCtx2 =
    let
        -- go through all the Usages in routCtx2
        -- insert any that were updated, into routCtx1
        poison location usage2 routCtxAccum =
            case Map.lookup location routCtx1 of
                Nothing ->
                    updateRoutCtx location usage2 routCtxAccum
                Just usage1 ->
                    -- it exists in both routCtxs.
                    -- if it is poisoned in either, it's poisoned here.
                    -- otherwise, it is OK to differ.
                    let
                        newUsage = case (usage1, usage2) of
                            (PoisonedWith _, _) -> usage1
                            (_, PoisonedWith _) -> usage2
                            _ -> usage1  -- or 2.  doesn't matter.
                    in
                        updateRoutCtx location newUsage routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx1 routCtx2
