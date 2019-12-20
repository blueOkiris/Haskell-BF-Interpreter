module Interpreter(runBFInterpreter) where

import Data.ByteString(ByteString)
import Data.ByteString.Char8(unpack)
import Debug.Trace(trace)
import Text.Read(readMaybe)
import Data.Char(chr, showLitChar)
import System.IO.Unsafe(unsafePerformIO)

import Loop(LoopPair(..), getLoopPairs, printPairs)

{-# NOINLINE readIntUnsafe #-}
readIntUnsafe :: Int -> Int
readIntUnsafe def =
    maybe def id $ readMaybe (unsafePerformIO $! getLine)

interpretBF :: [Char] -> [LoopPair] -> Int -> [Int] -> Int -> [IO()] -> [IO()]
interpretBF byteList loops pointer memory index instList =
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
                            (fst $ splitAt pointer memory) 
                                ++ [ ((memory !! newPointer) - 1) ]
                                    ++ (snd $ splitAt (newPointer + 1) memory)
                    else if byteList !! index == ',' then
                        if newPointer >= length memory then
                            memory ++ [ readIntUnsafe 0 ]
                        else
                            (fst $ splitAt pointer memory) 
                                ++ [ readIntUnsafe 0 ] 
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
    executeBFInstructions 0 $! outputs
    where
        programData = unpack bfFile
        loops = getLoopPairs programData
        --printLoops = trace (printPairs 0 loops) loops
        outputs = interpretBF programData loops 0 [0] 0 []
