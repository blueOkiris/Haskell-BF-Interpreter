{-
 - Author: Dylan Turner
 - Description: Set up environment and call interpreter
 -}

module Main where

import System.Environment(getArgs)
import System.IO(stdout, hSetBuffering, BufferMode(..))
import Data.Maybe(fromJust, fromMaybe)
import Text.Read(readMaybe)

import Parse(Parser(..), Stmt(..), getStmts, program)

data State = State  { tape      :: [Int]
                    , pointer   :: Int }

{-
 - "Double Apply"
 - Instead of (tape state) !! (pointer state),
 - do dapp tape !! pointer state
 -}
dapp :: (a -> b) -> (b -> c -> d) -> (a -> c) -> a -> d
dapp left op right val =
    op (left val) (right val)

setCell :: State -> Int -> State
setCell state newVal =
    let ptr = pointer state
        tp = tape state
        (pred, _:succ) = splitAt ptr tp in
    state { tape = pred ++ [ newVal ] ++ succ }

execute :: State -> [Stmt] -> IO State
execute state [] = do return state
execute state ((IoOp c):stmts)
    -- Print
    | c == '.' = do
        putStr $ (show $ dapp tape (!!) pointer state) ++ " "
        execute state stmts
    -- Input
    | otherwise = do -- ','
        input <- getLine
        let newState = setCell state $ fromMaybe 0 $ readMaybe input
        execute newState stmts
execute state ((Loop lb subStmts rb):stmts)
    -- Jump past ] if cell at pointer is 0
    | dapp tape (!!) pointer state == 0 = execute state stmts
    | otherwise = do
        -- Run the inner statements (including other loops)
        newState <- execute state subStmts
        if dapp tape (!!) pointer newState /= 0 then
            -- Repeat loop steps
            execute newState ((Loop lb subStmts rb):stmts)
        else
            -- Move on
            execute newState stmts
execute state _ = do return state

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if length args /= 1 then
        putStrLn "No filename provided!"
    else do
        code <- readFile $ args !! 0
        let (prog, leftOver) = fromJust $ runParser program code
        execute (State { tape = repeat 0, pointer = 0 }) (getStmts prog)
        putStrLn ""
