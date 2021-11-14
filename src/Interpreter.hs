-- Author: Dylan Turner
-- Description: Main part of bf interpreter

module Interpreter(runBFInterpreter, execBFCode) where

import Data.ByteString(ByteString)
import Data.ByteString.Char8(unpack)
import Debug.Trace(trace)
import Text.Read(readMaybe)
import Data.Char(chr, showLitChar)
import Data.List(elemIndex)
import System.IO.Unsafe(unsafePerformIO)

import Loop(LoopPair(..), getLoopPairs, printPairs)
import BFCmd(StateMachine(..), currMem, currCmd, showState)
import Lib

-- When the value doesn't require an IO operation (+, -, >, <, [, ])
intCmdNoIO :: StateMachine -> StateMachine
intCmdNoIO state
    | currCmd state == '+' =
        state   { memory =      lreplace memIndex memList (currMemValue + 1)                    -- Replace the state's currMem by currMem + 1
                , cmdIndex =    (cmdIndex state) + 1 }
    | currCmd state == '-' =
        state   { memory =      lreplace memIndex memList (currMemValue - 1)                    -- Replace the state's currMem by currMem - 1
                , cmdIndex =    (cmdIndex state) + 1 }
    | currCmd state == '>' =
        state   { pointer =     memIndex + 1
                , memory =      memIndex + 1 >= length memList ? memList ++ [ 0 ] :? memList    -- Move the pointer over to the right. If not big enough, add 0 to end
                , cmdIndex =    (cmdIndex state) + 1 }
    | currCmd state == '<' =
        state   { pointer =     memIndex > 0 ? memIndex - 1 :? memIndex                         -- Move the pointer to the left until at index 0
                , cmdIndex =    (cmdIndex state) + 1 }
    | currCmd state == '[' =
        state   { cmdIndex =    if (currMem state) == 0 then
                                    1 + (rightBracketIndex $ head $                             -- Jump PAST matching right bracket
                                        filter (\loop -> (leftBracketIndex loop) == (cmdIndex state)) (loops state))
                                else
                                    (cmdIndex state) + 1 }
    | currCmd state == ']' =
        state   { cmdIndex =    if (currMem state) /= 0 then
                                    leftBracketIndex $ head $                                   -- Jump BACK TO matching left bracket
                                        filter (\loop -> (rightBracketIndex loop) == (cmdIndex state)) (loops state)
                                else
                                    (cmdIndex state) + 1 }
    | otherwise = state   { cmdIndex = (cmdIndex state) + 1 }
    where
        memIndex =      pointer state
        memList =       memory  state
        currMemValue =  currMem state
        cmd =           currCmd state

execBFCode :: StateMachine -> IO()
execBFCode state
    | cmdIndex state == length (program state) = return ()
    | currCmd state == ',' =
        do
            input <- getLine
            execBFCode (state   { cmdIndex =    (cmdIndex state + 1)
                                , memory =      lreplace memIndex memList (maybe 0 id $ readMaybe input) })
    | currCmd state == '.' =
        do
            putStr $ (show $ currMem state) ++ " "
            execBFCode $ state { cmdIndex = (cmdIndex state + 1) }
    | otherwise = execBFCode $ intCmdNoIO state
    where
        memIndex =      pointer state
        memList =       memory  state
        currMemValue =  currMem state

runBFInterpreter :: ByteString -> IO()
runBFInterpreter bfFile =
    do
        execBFCode initialState
    where
        programData =   unpack bfFile
        progLoops =     getLoopPairs programData
        initialState =  StateMachine    { pointer = 0
                                        , memory =  [0]
                                        , loops = progLoops
                                        , program = programData
                                        , cmdIndex = 0 }
