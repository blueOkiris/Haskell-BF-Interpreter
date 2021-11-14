-- Author: Dylan Turner
-- Description: Helper functions for use throughout the program

module Lib(lreplace, Cond(..), (?), lremove) where

-- Replace list[index] with newItem
lreplace :: Int -> [a] -> a -> [a]
lreplace index list newItem =
    (fst $ splitAt index list) ++ [ newItem ] ++ (snd $ splitAt (index + 1) list)

-- Remove list[index]
lremove :: Int -> [a] -> [a]
lremove index list =
    (fst $ splitAt index list) ++ (snd $ splitAt (index + 1) list)

-- Ternary operator -> bool ? yes :? no
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
