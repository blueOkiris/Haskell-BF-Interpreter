module Main where

import System.Environment(getArgs)
import Data.ByteString(readFile)

import Interpreter(runBFInterpreter)

main :: IO ()
main =
    do
        args <- getArgs
        --putStrLn "Hello, world!"
        bfFile <- Data.ByteString.readFile 
                    (if length args /= 1 then
                        "No filename inputed"
                    else
                        args !! 0)
        runBFInterpreter bfFile
