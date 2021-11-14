{-
 - Author: Dylan Turner
 - Description:
 -  * Define a monad structure for parsing code
 -  * Then define the parsers for Brainf**k
 -}

-- First, what is a parser?

module Parse( Parser(..), Alternative(..)
            , program, stmt, loop, memOp, ptrOp, ioOp) where

-- Well, it's a function
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s -> do   -- Make use of Maybe monad
        (x', s') <- x s                     -- Run parser
        return (f x', s')

-- Chain parsers
instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (Parser f) <*> (Parser x) = Parser $ \s -> do
        (f', s1) <- f s
        (x', s2) <- x s1                    -- Chain unconsumed
        return (f' x', s2)

-- Get stuff from the parser
instance Monad Parser where
    (Parser x) >>= f = Parser $ \s -> do
        (x', s') <- x s                     -- Get value from og parser val
        runParser (f x') s'                 -- Then just run it and get maybe
    fail _ = Parser $ \s -> Nothing

-- Or, some, and many for our parsers
class (Applicative f) => Alternative f where
    empty   :: f a
    (<|>)   :: f a -> f a -> f a

    some :: f a -> f [a]
    some v =
        some_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

    many :: f a -> f [a]
    many v =
        many_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

instance Alternative Parser where
    empty = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Just x -> Just x
            Nothing -> y s

{-
- Parser for Brainf**k
- <stmt> ::= <mem-op> | <ptr-op> | <io-op> | <loop>
- <mem-op> ::= '+' | '-'
- <ptr-op> ::= '<' | '>'
- <io-op> ::= '.' | ','
- <loop> ::= '[' { <stmt> } ']'
-}

data Stmt = Op Char | Loop [Stmt]

instance Show Stmt where
    show (Op x) = "Char " ++ show x
    show (Loop l) = "Loop " ++ show l

char :: Char -> Parser Stmt
skip :: Parser Char
skipSome :: Parser [Char]
memOp :: Parser Stmt
ptrOp :: Parser Stmt
ioOp :: Parser Stmt
loop :: Parser Stmt
stmt :: Parser Stmt
program :: Parser [Stmt]

char c = Parser parser
    where
        parser [] = Nothing
        parser (x:xs)
            | x == c    = Just (Op c, xs)
            | otherwise = Nothing
skip = Parser parser
    where
        parser [] = Nothing
        parser (x:xs)
            | not (x `elem` "+-<>.,[]") = Just (x, xs)
            | otherwise = Nothing
skipSome = some skip

memOp = char '+' <|> char '-'
ptrOp = char '<' <|> char '-'
ioOp = char '.' <|> char ','
loop =  (Loop <$> ((\x y -> [ x, y ]) <$> char '[' <*> char ']'))
    <|> (Loop <$> ((\x y z -> x:y ++ [ z ]) <$> char '[' <*> many stmt <*> char ']'))
stmt = (skipSome *> stmt) <|> (memOp <|> ptrOp <|> ioOp <|> loop)

program = many stmt
