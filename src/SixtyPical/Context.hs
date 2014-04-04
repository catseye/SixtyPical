-- encoding: UTF-8

module SixtyPical.Context where

-- contexts for abstract interpretation.

import qualified Data.Map as Map

import SixtyPical.Model

--
-- The result of analyzing an instruction (or a block) is a map from
-- all relevant StorageLocations to how those StorageLocations were
-- used in that code (a Usage.)
--
-- If a StorageLocation is missing from the map, we can assume that
-- that code does not affect that StorageLocation (it is "retained".)
--

data Usage = PoisonedWith StorageLocation
           | UpdatedWith StorageLocation
           | NotChanged
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
        -- go through all the Usages in the calledRoutCtx
        -- insert any that were updated, into routCtx
        poison location usage routCtxAccum =
            case usage of
                UpdatedWith ulocation ->
                    Map.insert location (PoisonedWith ulocation) routCtxAccum
    in
        Map.foldrWithKey (poison) routCtx calledRoutCtx


ppAnalysis :: ProgramContext -> IO ()
ppAnalysis progCtx =
    let
        li = Map.toList progCtx
    in do
        ppRoutines li

ppRoutines [] = return ()
ppRoutines ((name, routCtx):rest) = do
    putStrLn $ name
    ppRoutine routCtx
    putStrLn ""
    ppRoutines rest

ppRoutine routCtx =
    let
        li = Map.toList routCtx
    in do
        ppUsages li

ppUsages [] = return ()
ppUsages ((loc, usage):rest) = do
    putStrLn $ ("  " ++ (show loc) ++ ": " ++ (show usage))
    ppUsages rest
