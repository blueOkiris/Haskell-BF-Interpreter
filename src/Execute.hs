{-
 - Author: Dylan Turner
 - Description: Given a set of statements, execute them and update State
 -}

module Execute(State(..), execute) where

import Data.Maybe(fromMaybe)
import Text.Read(readMaybe)
import Data.Aviary.Birds(phoenix)

import Parse(Stmt(..))

-- Machine State
data State = State  { tape      :: [Int]
                    , pointer   :: Int }

-- Sets the current cell of a state to a new value
setCell :: State -> Int -> State
setCell state newVal =
    let ptr = pointer state
        tp = tape state
        (pred, _:succ) = splitAt ptr tp in
    state { tape = pred ++ [ newVal ] ++ succ }

-- Execute parsed code
execute :: State -> [Stmt] -> IO State
execute state [] = do return state
execute state ((IoOp c):stmts)
    -- Print
    | c == '.' = do
        putStr $ (show $ phoenix (!!) tape pointer state) ++ " "
        execute state stmts
    -- Input
    | otherwise = do -- ','
        input <- getLine
        let newState = setCell state $ fromMaybe 0 $ readMaybe input
        execute newState stmts
-- Add or subtract the memory in the current cell
execute state ((MemOp c):stmts) =
    let op = if c == '+' then (+) else (-)
        curVal = phoenix (!!) tape pointer state
        newState = setCell state (op curVal 1) in
        execute newState stmts
-- Move the pointer
execute state ((PtrOp c):stmts) =
    let op = if c == '>' then (+) else (-)
        newState = state { pointer = op (pointer state) 1 } in
        execute newState stmts
-- Handle loop structures
execute state ((Loop lb subStmts rb):stmts)
    -- Jump past ] if cell at pointer is 0
    | phoenix (!!) tape pointer state == 0 = execute state stmts
    | otherwise = do
        -- Run the inner statements (including other loops)
        newState <- execute state subStmts
        if phoenix (!!) tape pointer newState /= 0 then
            -- Repeat loop steps
            execute newState ((Loop lb subStmts rb):stmts)
        else
            -- Move on
            execute newState stmts
