-- encoding: UTF-8

module SixtyPical.Checker where

import SixtyPical.Model
import SixtyPical.Transformer

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

consistentInitialTableSizes p@(Program decls routines) =
    let
        inconsistentTableSizes = foldProgramDecls (checkDecl) 0 p
    in
        inconsistentTableSizes == 0
    where
        checkDecl (Reserve _ (ByteTable sz) []) acc = acc
        checkDecl (Reserve _ (ByteTable sz) vals) acc =
            case sz == (length vals) of
                True -> acc
                False -> acc + 1
        checkDecl _ acc = acc

-- - - - - - -

checkAndTransformProgram :: Program -> Maybe Program
checkAndTransformProgram program =
    if
        trueOrDie "missing 'main' routine" (routineDeclared "main" program) &&
        trueOrDie "duplicate location name" (noDuplicateDecls program) &&
        trueOrDie "duplicate routine name" (noDuplicateRoutines program) &&
        trueOrDie "undeclared routine" (noUseOfUndeclaredRoutines program) &&
        trueOrDie "indexed access of non-table" (noIndexedAccessOfNonTables program)  &&
        trueOrDie "initial table incorrect size" (consistentInitialTableSizes program) 
      then
        let
            program' = numberProgramLoops program
            program'' = renameBlockDecls program'
            program''' = liftBlockDecls program''
            program'''' = fillOutNamedLocationTypes program'''
        in
            Just program''''
      else Nothing
