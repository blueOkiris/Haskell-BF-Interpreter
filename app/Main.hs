module Main where

import System.Environment(getArgs)
import Data.ByteString(readFile)
import System.IO(stdout, hSetBuffering, BufferMode(..))

import Interpreter(runBFInterpreter)

main :: IO ()
main =
    do
        hSetBuffering stdout NoBuffering
        args <- getArgs
        --putStrLn "Hello, world!"
        bfFile <- Data.ByteString.readFile 
                    (if length args /= 1 then
                        "No filename inputed"
                    else
                        args !! 0)
        runBFInterpreter bfFile
