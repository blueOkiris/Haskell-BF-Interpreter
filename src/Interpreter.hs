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

intCmdNoIO :: StateMachine -> StateMachine
intCmdNoIO state =
    if currCmd state == '+' then
        -- Replace the state's currMem by currMem + 1
        state   { memory =      (fst $ splitAt (pointer state) (memory state)) 
                                    ++ [ (currMem state) + 1 ] 
                                        ++ (snd $ splitAt ((pointer state) + 1) (memory state))
                , cmdIndex =    (cmdIndex state) + 1 }
    else if currCmd state == '-' then
        -- Replace the state's currMem by currMem - 1
        state   { memory =      (fst $ splitAt (pointer state) (memory state)) 
                                    ++ [ (currMem state) - 1 ] 
                                        ++ (snd $ splitAt ((pointer state) + 1) (memory state))
                , cmdIndex =    (cmdIndex state) + 1 }
    else if currCmd state == '>' then
        -- Replace the state's currMem by currMem - 1
        state   { pointer =     (pointer state) + 1
                , memory =      if ((pointer state) + 1) >= (length (memory state)) then
                                    (memory state) ++ [ 0 ]
                                else
                                    (memory state) 
                , cmdIndex =    (cmdIndex state) + 1 }
    else if currCmd state == '<' then
        -- Replace the state's currMem by currMem - 1
        state   { pointer =     if (pointer state) > 0 then
                                    (pointer state) - 1
                                else
                                    pointer state
                , cmdIndex =    (cmdIndex state) + 1 }
    else if currCmd state == '[' then
        state   { cmdIndex =    if (currMem state) == 0 then
                                    -- Jump PAST matching right bracket
                                    1 + (rightBracketIndex $ head $
                                        filter (\loop -> (leftBracketIndex loop) == (cmdIndex state)) (loops state))
                                else
                                    (cmdIndex state) + 1 }
    else if currCmd state == ']' then
        state   { cmdIndex =    if (currMem state) /= 0 then
                                    -- Jump BACK TO matching left bracket
                                    leftBracketIndex $ head $
                                        filter (\loop -> (rightBracketIndex loop) == (cmdIndex state)) (loops state)
                                else
                                    (cmdIndex state) + 1 }
    else
        state   { cmdIndex =    (cmdIndex state) + 1 }

execBFCode :: StateMachine -> IO()
execBFCode state =
    if cmdIndex state == length (program state) then
        putStrLn "\nDone."
    else if currCmd state == ',' then
        do
            input <- getLine
            let inputInt :: Int; inputInt = maybe 0 id $ readMaybe input
            let newState = state    { cmdIndex =    (cmdIndex state + 1)
                                    , memory =      (fst $ splitAt (pointer state) (memory state)) 
                                                        ++ [ inputInt ]
                                                            ++ (snd $ splitAt ((pointer state) + 1) (memory state)) }
            execBFCode newState
    else if currCmd state == '.' then
        do
            id $! putStr $ show $ currMem state
            execBFCode $ state { cmdIndex = (cmdIndex state + 1) }
    else
        execBFCode $ intCmdNoIO state

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
