module Loop(getLoopPairs, LoopPair(..), printPairs, showLoops) where

import Lib(lremove)

-- This module finds and pairs all the left and right brackets for easier jumping

data LoopPair =
    LoopPair    { leftBracketIndex  :: Int
                , rightBracketIndex :: Int }

showLoops :: [LoopPair] -> String
showLoops loops =
    show (map (\lp -> (leftBracketIndex lp, rightBracketIndex lp)) loops)

printPairs :: Int -> [LoopPair] -> String
printPairs index loops
    | length loops == 0 =           "[]"
    | length loops == 1 =           "[ (" ++ li ++ ", " ++ ri ++ ") ]"
    | index == 0 =                  "[ (" ++ li ++ ", " ++ ri ++ ")" ++ (printPairs (index + 1) loops)
    | index == (length loops) - 1 = " (" ++ li ++ ", " ++ ri ++ ") ]"
    | otherwise =                   " (" ++ li ++ ", " ++ ri ++ ")" ++ (printPairs (index + 1) loops)
    where
        li = show $ leftBracketIndex $ loops !! index
        ri = show $ rightBracketIndex $ loops !! index

getLeftPairs :: Int -> [Char] -> [LoopPair]
getLeftPairs index program
    | index == length program = []
    | program !! index == '[' =
        [ LoopPair  { leftBracketIndex =    index
                    , rightBracketIndex =   -1 } ]
            ++ getLeftPairs (index + 1) program
    | otherwise = getLeftPairs (index + 1) program

getRightPairs :: [Char] -> Int -> Int -> [LoopPair] -> [LoopPair]
getRightPairs program index loopIndex pairs
    | index == length program = pairs
    | program !! index == '[' = getRightPairs program (index + 1) (loopIndex + 1) pairs
    | program !! index == ']' =
        -- Change and move to end (remove and put copy at end)
        -- Decrease counter
        getRightPairs program (index + 1) (loopIndex - 1) $
            (lremove loopIndex pairs) ++ [ ((pairs !! loopIndex) { rightBracketIndex = index }) ]
    | otherwise = getRightPairs program (index + 1) loopIndex pairs

getLoopPairs :: [Char] -> [LoopPair]
getLoopPairs program =
    --trace (printPairs 0 leftPairs)
    rightPairs
    where
        leftPairs = getLeftPairs 0 program
        rightPairs = getRightPairs program 0 (-1) leftPairs
        