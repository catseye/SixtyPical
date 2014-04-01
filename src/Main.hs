-- encoding: UTF-8

module Main where

import System.IO
import System.Environment
import System.Exit

import SixtyPical.Model
import SixtyPical.Parser (parseProgram)
import SixtyPical.Checker (checkAndTransformProgram)
import SixtyPical.Analyzer (analyzeProgram)
import SixtyPical.Emitter (emitProgram)

-- -- -- -- driver -- -- -- --

usage = do
    putStrLn "Usage: sixtypical (parse|check|analyze|emit) filename.60pical"
    exitWith $ ExitFailure 1

checkProgram p =
    case checkAndTransformProgram p of
        Just newprog ->
            True

main = do
    args <- getArgs
    case args of
        [verb, filename] -> do
            programText <- readFile filename
            case (verb, parseProgram programText) of
                ("parse", Right program) -> do
                    putStrLn $ show $ program
                ("check", Right program) -> do
                    putStrLn $ show $ checkProgram program
                ("analyze", Right program) ->
                    case checkAndTransformProgram program of
                        Just newprog ->
                            putStrLn $ show $ analyzeProgram newprog
                ("emit", Right program) ->
                    case checkAndTransformProgram program of
                        Just newprog ->
                            case analyzeProgram newprog of
                                _ ->
                                    putStr $ emitProgram newprog
                (_, Left problem) -> do
                    hPutStrLn stderr (show problem)
                    exitWith $ ExitFailure 1
                (_, _) -> usage
        _ -> usage
