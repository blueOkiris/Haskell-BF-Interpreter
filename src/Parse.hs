{-
 - Author: Dylan Turner
 - Description:
 -  * Define a monad structure for parsing code
 -  * Then define the parsers for Brainf**k
 -}

-- First, what is a parser?

module Parse( Parser(..), Stmt(..), Program(..)
            , getStmts, program) where

-- Well, it's a function
newtype Parser a =
    Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap func (Parser x) = Parser $ \str -> do
        (x', str') <- x str                 -- Unwrap from maybe
        return (func x', str')

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
        (x', s') <- x s                     -- Get val from og
        runParser (f x') s'                 -- Run it and get maybe
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

data Stmt = MemOp Char  | PtrOp Char | IoOp Char
          | Loop Char [Stmt] Char deriving Show
data Program = Program [Stmt] deriving Show

getStmts :: Program -> [Stmt]
getStmts (Program stmts) = stmts

char :: Char -> Parser Char
skip :: Parser Char
skipMany :: Parser [Char]
memOp :: Parser Stmt
ptrOp :: Parser Stmt
ioOp :: Parser Stmt
loop :: Parser Stmt
stmt :: Parser Stmt
program :: Parser Program

char c =
    let parser [] = Nothing
        parser (x:xs)
            | x == c    = Just (c, xs)
            | otherwise = Nothing in
    Parser parser
skip =
    let parser [] = Nothing
        parser (x:xs)
            | not (x `elem` "+-<>.,[]") = Just (x, xs)
            | otherwise = Nothing in
    Parser parser
skipMany = many skip

memOp = MemOp <$> (char '+' <|> char '-')
ptrOp = PtrOp <$> (char '<' <|> char '>')
ioOp = IoOp <$> char '.' <|> char ','
loop = do
    lb <- char '['
    stmts <- many $ skipMany *> stmt
    rb <- skipMany *> char ']'
    return $ Loop lb stmts rb

stmt = memOp <|> ptrOp <|> ioOp <|> loop

program = Program <$> (many $ skipMany *> stmt)
