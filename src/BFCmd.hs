module BFCmd(StateMachine(..), currMem, currCmd, showState) where

import Loop(LoopPair(..), showLoops)

data StateMachine =
    StateMachine    { pointer   :: Int
                    , memory    :: [Int]
                    , loops     :: [LoopPair] 
                    , program   :: [Char]
                    , cmdIndex  :: Int }

showState :: StateMachine -> String
showState state = 
    "[ pointer =\t" ++ (show $ pointer state)
        ++ "\n, memory =\t" ++ (show $ memory state)
            ++ "\n, loops =\t" ++ (showLoops $ loops state)
                ++ "\n, program =\t" ++ (show $ program state)
                    ++ "\n, cmdIndex =\t" ++ (show $ cmdIndex state) ++ " ]"

currMem :: StateMachine -> Int
currMem state =
    (memory state) !! (pointer state)

currCmd :: StateMachine -> Char
currCmd state =
    (program state) !! (cmdIndex state)