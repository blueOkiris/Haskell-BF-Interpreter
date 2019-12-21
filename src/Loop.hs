module Loop(getLoopPairs, LoopPair(..), printPairs, showLoops) where

-- This module finds and pairs all the left and right brackets for easier jumping

import Debug.Trace

data LoopPair =
    LoopPair    { leftBracketIndex  :: Int
                , rightBracketIndex :: Int }

showLoops :: [LoopPair] -> String
showLoops loops =
    show (map (\lp -> (leftBracketIndex lp, rightBracketIndex lp)) loops)

printPairs :: Int -> [LoopPair] -> String
printPairs index loops =
    --trace ("Index: " ++ (show index)) $
    if length loops == 0 then
        "[]"
    else if length loops == 1 then
        "[ (" ++ li ++ ", " ++ ri ++ ") ]"
    else if index == 0 then
        "[ (" ++ li ++ ", " ++ ri ++ ")"
            ++ (printPairs (index + 1) loops)
    else if index == (length loops) - 1 then
        " (" ++ li ++ ", " ++ ri ++ ") ]"
    else
        " (" ++ li ++ ", " ++ ri ++ ")"
            ++ (printPairs (index + 1) loops)
    where
        li = show $ leftBracketIndex $ loops !! index
        ri = show $ rightBracketIndex $ loops !! index

getLeftPairs :: Int -> [Char] -> [LoopPair]
getLeftPairs index program =
    if index == length program then
        []
    else if program !! index == '[' then
        [ LoopPair  { leftBracketIndex =    index
                    , rightBracketIndex =   -1 } ]
            ++ getLeftPairs (index + 1) program
    --else if program !! index == ']' then

    else
        getLeftPairs (index + 1) program

getRightPairs :: [Char] -> Int -> Int -> [LoopPair] -> [LoopPair]
getRightPairs program index loopIndex pairs =
    --trace ((printPairs 0 pairs) ++ " with loop index: " ++ (show loopIndex) ++ " at program index: " ++ (show index)) $
    if index == length program then
        pairs
    else if program !! index == '[' then
        getRightPairs program (index + 1) (loopIndex + 1) pairs
    else if program !! index == ']' then
        -- Change and move to end
        -- Decrease counter
        getRightPairs program (index + 1) (loopIndex - 1) $!
            (fst $ splitAt loopIndex pairs) 
                ++ (snd $ splitAt (loopIndex + 1) pairs)
                    ++ [ ((pairs !! loopIndex) { rightBracketIndex = index }) ]
    else
        getRightPairs program (index + 1) loopIndex pairs

getLoopPairs :: [Char] -> [LoopPair]
getLoopPairs program =
    --trace (printPairs 0 leftPairs)
    rightPairs
    where
        leftPairs = getLeftPairs 0 program
        rightPairs = getRightPairs program 0 (-1) leftPairs
        