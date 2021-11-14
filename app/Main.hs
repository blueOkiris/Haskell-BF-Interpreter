{-
 - Author: Dylan Turner
 - Description: Set up environment and call interpreter
 -}

module Main where

import System.Environment(getArgs)
import System.IO(stdout, hSetBuffering, BufferMode(..))

import Parse( Parser(..), Alternative(..)
            , program, stmt, loop, memOp, ptrOp, ioOp)

interpret :: String -> IO()
interpret code = do
    putStrLn $ show code
    let stmts = runParser program code
    putStrLn $ show stmts

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if length args /= 1 then
        putStrLn "No filename provided!"
    else do
        bfFile <- readFile $ args !! 0
        interpret bfFile
