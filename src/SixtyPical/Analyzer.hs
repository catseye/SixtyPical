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
          checkBlock name instrs progCtx routCtx
      
      checkBlock nm [] progCtx routCtx = routCtx
      checkBlock nm (instr:instrs) progCtx routCtx =
          let
              routCtx' = checkInstr nm instr progCtx routCtx
          in
              checkBlock nm instrs progCtx routCtx'
      
      -- -- -- -- -- -- -- -- -- -- -- --
      
      checkInstr nm (COPY src dst) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith src) routCtx
      checkInstr nm (DELTA dst val) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith (Immediate val)) routCtx
      checkInstr nm (ADD dst src) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith src) routCtx
      checkInstr nm (SUB dst src) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith src) routCtx

      checkInstr nm (AND dst src) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith src) routCtx
      checkInstr nm (OR dst src) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith src) routCtx
      checkInstr nm (XOR dst src) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith src) routCtx

      checkInstr nm (JSR name) progCtx routCtx =
          let
              Just calledRout = lookupRoutine program name
          in
              case Map.lookup name progCtx of
                  Just calledRoutCtx ->
                      mergeRoutCtxs nm routCtx calledRoutCtx calledRout
                  Nothing ->
                      error ("can't call routine '" ++ name ++ "' before it is defined")
      checkInstr nm (CMP reg addr) progCtx routCtx =
          -- TODO: mark Carry bit as "touched" here
          routCtx
      checkInstr nm (IF _ branch b1 b2) progCtx routCtx =
          let
              routCtx1 = checkBlock nm b1 progCtx routCtx
              routCtx2 = checkBlock nm b2 progCtx routCtx
          in
              mergeAlternateRoutCtxs nm routCtx1 routCtx2
      checkInstr nm (REPEAT _ branch blk) progCtx routCtx =
          -- TODO: oooh, this one's gonna be fun too
          --checkBlock blk progCtx routCtx
          routCtx

      -- TODO -- THESE ARE WEAK --
      checkInstr nm (WITH _ blk) progCtx routCtx =
          checkBlock nm blk progCtx routCtx

      checkInstr nm (BIT dst) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith (Immediate 0)) routCtx

      checkInstr nm (SHR dst flg) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith flg) routCtx
      checkInstr nm (SHL dst flg) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith flg) routCtx

      checkInstr nm (COPYROUTINE name dst) progCtx routCtx =
          updateRoutCtx nm dst (UpdatedWith (Immediate 7)) routCtx

      checkInstr nm (JMPVECTOR dst) progCtx routCtx =
          routCtx

      checkInstr nm NOP progCtx routCtx =
          routCtx

      checkInstr nm instr _ _ = error (
          "Internal error: sixtypical doesn't know how to " ++
          "analyze '" ++ (show instr) ++ "' in '" ++ nm ++ "'")

--
-- Utility function:
-- Take 2 routine contexts -- the current routine and a routine that was just
-- JSR'ed to (immediately previously) -- and merge them to create a new
-- context for the current routine.
--
mergeRoutCtxs nm routCtx calledRoutCtx calledRout@(Routine name outputs _) =
    let
        -- go through all the Usages in the calledRoutCtx
        -- insert any that were updated, into routCtx
        poison location usage routCtxAccum =
            case usage of
                UpdatedWith ulocation ->
                    case location `elem` outputs of
                        True ->
                            updateRoutCtx nm location usage routCtxAccum
                        False ->
                            updateRoutCtx nm location (PoisonedWith ulocation) routCtxAccum
                PoisonedWith ulocation ->
                    updateRoutCtx nm location usage routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx calledRoutCtx

--
-- Utility function:
-- Take 2 routine contexts -- one from each branch of an `if` -- and merge
-- them to create a new context for the remainder of the routine.
--
-- We use a weaker version of updateRoutCtx to build the merged context.
-- We do this because accessing a poisoned storage location from either
-- of the branch contexts is not an error at the merge point -- we simply
-- make the storage location poisoned in the resulting context.  (If the
-- poisoned location is accessed subsequently to the merge point, that is
-- of course still an error.)
--
mergeAlternateRoutCtxs nm routCtx1 routCtx2 =
    let
        -- go through all the Usages in routCtx2
        -- insert any that were updated, into routCtx1
        poison location usage2 routCtxAccum =
            case Map.lookup location routCtx1 of
                Nothing ->
                    updateRoutCtx nm location usage2 routCtxAccum
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
                        updateRoutCtx nm location newUsage routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx1 routCtx2
    where
        -- a weaker version of updateRoutCtx, which does not error if
        -- we access a poisoned source
        updateRoutCtx nm dst (UpdatedWith src) routCtx =
            let
                s = untypedLocation src
                d = untypedLocation dst
            in
                Map.insert d (UpdatedWith s) routCtx
        updateRoutCtx nm dst (PoisonedWith src) routCtx =
            Map.insert (untypedLocation dst) (PoisonedWith $ untypedLocation src) routCtx
