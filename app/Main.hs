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

interpret :: String -> IO()
interpret code = do
    --putStrLn $ show code
    let (prog, leftOver) = fromJust $ runParser program code
    --putStrLn $ show prog
    execute (State { tape = repeat 0, pointer = 0 }) (getStmts prog)
    putStrLn ""

dapp :: (a -> b) -> (b -> c -> d) -> (a -> c) -> a -> d
dapp left op right val =
    op (left val) (right val)

setCell :: State -> Int -> State
setCell state newVal =
    let ptr = pointer state
        tp = tape state
        (pred, _:succ) = splitAt ptr tp in
    state { tape = pred ++ [ newVal ] ++ succ }

execute :: State -> [Stmt] -> IO()
execute _ [] = do return ()
execute state ((IoOp c):stmts)
    | c == '.' = do
        putStr $ (show $ dapp (tape) (!!) (pointer) state) ++ " "
        execute state stmts
    | otherwise = do -- ','
        input <- getLine
        let newState = setCell state $ fromMaybe 0 $ readMaybe input
        execute newState stmts
execute _ _ = do return ()

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if length args /= 1 then
        putStrLn "No filename provided!"
    else do
        bfFile <- readFile $ args !! 0
        interpret bfFile
