{-
 - Author: Dylan Turner
 - Description: Set up environment and call interpreter
 -}

module Main where

import System.Environment(getArgs)
import System.IO(stdout, hSetBuffering, BufferMode(..))
import Data.Maybe(fromJust)

import Parse(Parser(..), getStmts, program)
import Execute(State(..), execute)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if length args /= 1 then
        putStrLn "No filename provided!"
    else do
        code <- readFile $ args !! 0
        let (prog, leftOver) = fromJust $ runParser program code
        execute (State { tape = repeat 0, pointer = 0 }) (getStmts prog)
        putStrLn ""
