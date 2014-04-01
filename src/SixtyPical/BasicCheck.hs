-- encoding: UTF-8

module SixtyPical.BasicCheck where

import SixtyPical.Model

routineDeclared routName (Program _ routines) =
    elem routName (map (getRoutineName) routines)
    where
        getRoutineName (Routine name _) = name

locationDeclared locName (Program decls _) =
    elem locName (map (getLocationName) decls)
    where
        getLocationName (Assign name _ _) = name
        getLocationName (Reserve name _) = name

mainDeclared program =
    if
        routineDeclared "main" program
      then
        True
      else
        error "missing 'main' routine"

allUsedLocationsDeclared p@(Program _ routines) =
    True

checkProgram program =
    mainDeclared program && allUsedLocationsDeclared program
