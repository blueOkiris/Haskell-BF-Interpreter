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

--{-# NOINLINE readIntUnsafe #-}
--readIntUnsafe :: Int -> Int
--readIntUnsafe def =
--    maybe def id $ readMaybe (unsafePerformIO $! getLine)

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
            let inputInt :: Int; inputInt = maybe 0 id $! readMaybe input
            let newState = state    { cmdIndex =    (cmdIndex state + 1)
                                    , memory =      (fst $ splitAt (pointer state) (memory state)) 
                                                        ++ [ inputInt ]
                                                            ++ (snd $ splitAt ((pointer state) + 1) (memory state)) }

            execBFCode newState
    else if currCmd state == '.' then
        do
            putStr $ show $ currMem state
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


{-interpretBF :: [Char] -> Int -> StateMachine -> IO()
interpretBF cmds cmdIndex state =
    if cmdIndex == length cmds then
        return ()
    else if cmds !! cmdIndex == ',' then
        do
            memValueStr <- getLine
            let memValue = maybe 178 id $! readMaybe memValueStr
            interpretBF cmds newIndex $!
                state   { pointer = newPointer
                        , memory =
                            if newPointer >= length newMemory then
                                newMemory ++ [ memValue ]
                            else
                                (fst $ splitAt newPointer newMemory) ++ [ memValue ] ++ (snd $ splitAt (newPointer + 1) newMemory) }
    else if cmds !! cmdIndex == '.' then
        do
            -- Execute immediately
            id $! putStr $ show (newMemory !! newPointer)--[ chr (newMemory !! newPointer) ]
            interpretBF cmds newIndex $
                state   { pointer = newPointer
                        , memory = newMemory }
    else if (cmds !! cmdIndex) == '+' || (cmds !! cmdIndex) == '-'
        || (cmds !! cmdIndex) == '>' || (cmds !! cmdIndex) == '<' then
            interpretBF cmds newIndex $
                state   { pointer = newPointer
                        , memory =  newMemory }
    else
        interpretBF cmds newIndex state
    where
        newPointer =
            if cmds !! cmdIndex == '>' then
                (pointer state) + 1
            else if cmds !! cmdIndex == '<' && (pointer state) > 0 then
                (pointer state) - 1
            else
                (pointer state)
        
        newMemory = if cmds !! cmdIndex == '+' then
                        if newPointer >= length (memory state) then
                            (memory state) ++ [1]
                        else
                            (fst $ splitAt newPointer (memory state)) 
                                ++ [ (((memory state) !! newPointer) + 1) ] 
                                    ++ (snd $ splitAt (newPointer + 1) (memory state))
                    else if cmds !! cmdIndex == '-' then
                        if newPointer >= length (memory state) then
                            (memory state) ++ [-1]
                        else
                            (fst $ splitAt (pointer state) (memory state)) 
                                ++ [ (((memory state) !! newPointer) - 1) ]
                                    ++ (snd $ splitAt (newPointer + 1) (memory state))
                    else
                        if newPointer >= length (memory state) then
                            (memory state) ++ [0]
                        else
                            (memory state)
        currLoopPairL = filter (\loop -> (leftBracketIndex loop) == cmdIndex) (loops state)
        currLoopPairR = filter (\loop -> (rightBracketIndex loop) == cmdIndex) (loops state)
        newIndex =  if cmds !! cmdIndex == '[' && newMemory !! newPointer == 0 then
                        (rightBracketIndex (currLoopPairL !! 0)) + 1
                    else if cmds !! cmdIndex == ']' && newMemory !! newPointer /= 0 then
                        leftBracketIndex (currLoopPairR !! 0)
                    else
                        cmdIndex + 1

interpretBF :: [Char] -> [LoopPair] -> Int -> [Int] -> Int -> [IO()] -> [IO()]
interpretBF byteList loops memory state index instList =
    if index == (length byteList) then
        instList
    else
        interpretBF byteList loops newPointer newMemory newIndex newList
    where
        newPointer =
                    if byteList !! index == '>' then
                        pointer + 1
                    else if byteList !! index == '<' && pointer > 0 then
                        pointer - 1
                    else
                        pointer
        
        userInput = if byteList !! index == ',' then
                        (do
                            str <- getLine
                            return str)
                    else
                        ""

        newMemory = if byteList !! index == '+' then
                        if newPointer >= length memory then
                            memory ++ [1]
                        else
                            (fst $ splitAt newPointer memory) 
                                ++ [ ((memory !! newPointer) + 1) ] 
                                    ++ (snd $ splitAt (newPointer + 1) memory)
                    else if byteList !! index == '-' then
                        if newPointer >= length memory then
                            memory ++ [-1]
                        else
                            (fst $ splitAt memory state) 
                                ++ [ ((memory !! newPointer) - 1) ]
                                    ++ (snd $ splitAt (newPointer + 1) memory)
                    else if byteList !! index == ',' then
                        if newPointer >= length memory then
                            memory ++ [ maybe 0 id $! readMaybe userInput ]
                        else
                            (fst $ splitAt memory state) 
                                ++ [ maybe 0 id $! readMaybe userInput ] 
                                    ++ (snd $ splitAt (newPointer + 1) memory)
                    else
                        if newPointer >= length memory then
                            memory ++ [0]
                        else
                            memory

        newList =   if byteList !! index == '.' then
                        instList ++ [ putStr $! "" ++ [ chr (newMemory !! newPointer) ] ]
                    else
                        instList
        
        currLoopPairL = filter (\loop -> (leftBracketIndex loop) == index) loops
        currLoopPairR = filter (\loop -> (rightBracketIndex loop) == index) loops
        newIndex =  if byteList !! index == '[' && newMemory !! newPointer == 0 then
                        (rightBracketIndex (currLoopPairL !! 0)) + 1
                    else if byteList !! index == ']' && newMemory !! newPointer /= 0 then
                        leftBracketIndex (currLoopPairR !! 0)
                    else
                        index + 1

executeBFInstructions :: Int -> [IO()] -> IO()
executeBFInstructions index instList =
    --trace ("Instruction #" ++ (show index) ++ " out of " ++ (show $ length instList)) $
    if index == (length instList) then
        putStr ""
    else
        do
            (instList !! index)
            executeBFInstructions (index + 1) instList

runBFInterpreter :: ByteString -> IO()
runBFInterpreter bfFile =
    outputs
    where
        programData = unpack bfFile
        progLoops = getLoopPairs programData
        --printLoops = trace (printPairs 0 loops) loops
        outputs = interpretBF programData 0 $
                    StateMachine    { pointer = 0
                                    , memory =  [0]
                                    , loops = progLoops }
-}
