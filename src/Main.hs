-- encoding: UTF-8

module Main where

import System.IO
import System.Environment
import System.Exit

import SixtyPical.Model
import SixtyPical.Parser (parseProgram)
import SixtyPical.BasicCheck (checkProgram)
import SixtyPical.Context (analyzeProgram)

-- -- -- -- driver -- -- -- --

usage = do
    putStrLn "Usage: sixtypical (parse|check|analyze) filename.60pical"
    exitWith $ ExitFailure 1

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
                ("analyze", Right program) -> do
                    putStrLn $ show $ analyzeProgram program
                (_, Left problem) -> do
                    hPutStrLn stderr (show problem)
                    exitWith $ ExitFailure 1
                (_, _) -> usage
        _ -> usage
