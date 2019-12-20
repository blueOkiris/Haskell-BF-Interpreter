module BFCmd where

import Loop(LoopPair(..))

data StateMachine =
    StateMachine    { pointer :: Int
                    , memory  :: [Int]
                    , loops   :: [LoopPair] }