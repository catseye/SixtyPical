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

untypedLocation (HighByteOf x) =
    untypedLocation x
untypedLocation (LowByteOf x) =
    untypedLocation x
untypedLocation (Indexed table index) =
    untypedLocation table
untypedLocation (NamedLocation _ name) =
    NamedLocation Nothing name
untypedLocation x = x

updateRoutCtx :: String -> StorageLocation -> Usage -> RoutineContext -> RoutineContext
updateRoutCtx nm dst (UpdatedWith src) routCtx =
    let
        s = untypedLocation src
        d = untypedLocation dst
    in
        case Map.lookup s routCtx of
            Just (PoisonedWith _) ->
                error ("routine '" ++ nm ++ "' does not preserve '" ++
                       (show s) ++ "' (in context: " ++ (show routCtx) ++ ")")
            _ ->
                Map.insert d (UpdatedWith s) routCtx
updateRoutCtx nm dst (PoisonedWith src) routCtx =
    Map.insert (untypedLocation dst) (PoisonedWith $ untypedLocation src) routCtx
                  
-- pretty printing

ppAnalysis :: Program -> ProgramContext -> IO ()
ppAnalysis program progCtx =
    let
        li = Map.toList progCtx
    in do
        ppRoutines program li

ppRoutines program [] = return ()
ppRoutines program ((name, routCtx):rest) =
    let
        Just (Routine rname outputs _) = lookupRoutine program name
    in do
        putStrLn (rname ++ " (" ++ (show outputs) ++ ")")
        ppRoutine routCtx
        putStrLn ""
        ppRoutines program rest

ppRoutine routCtx =
    let
        li = Map.toList routCtx
    in do
        ppUsages li

ppUsages [] = return ()
ppUsages ((loc, usage):rest) = do
    putStrLn $ ("  " ++ (show loc) ++ ": " ++ (show usage))
    ppUsages rest
