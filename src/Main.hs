-- encoding: UTF-8

module Main where

import System.IO
import System.Environment
import System.Exit

import SixtyPical.Parser (parseProgram)
import SixtyPical.Model
import SixtyPical.Context (checkProgram)

-- -- -- -- driver -- -- -- --

usage = do
    putStrLn "Usage: sixtypical (parse|check) filename.60pical"
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
                (_, Left problem) -> do
                    hPutStrLn stderr (show problem)
                    exitWith $ ExitFailure 1
                (_, _) -> usage
        _ -> usage
