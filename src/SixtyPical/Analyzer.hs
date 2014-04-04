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
      
      checkInstr (COPY src dst) progCtx routCtx =
          case Map.lookup src routCtx of
              Just (PoisonedWith _) ->
                  error ("routine does not preserve '" ++ (show src) ++ "'")
              _ ->
                  Map.insert dst (UpdatedWith src) routCtx
      checkInstr (DELTA dst val) progCtx routCtx =
          -- TODO check that dst is not poisoned
          Map.insert dst (UpdatedWith (Immediate val)) routCtx
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
          -- TODO: oooh, this one's gonna be fun
          --checkBlock b1 progCtx routCtx
          --checkBlock b2 progCtx routCtx
          routCtx
      checkInstr (REPEAT _ branch blk) progCtx routCtx =
          -- TODO: oooh, this one's gonna be fun too
          --checkBlock blk progCtx routCtx
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
                    case (untypedLocation location) `elem` outputs of
                        True ->
                            Map.insert location usage routCtxAccum
                        False ->
                            Map.insert location (PoisonedWith ulocation) routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx calledRoutCtx

untypedLocation (NamedLocation (Just _) name) =
    NamedLocation Nothing name
untypedLocation x = x

