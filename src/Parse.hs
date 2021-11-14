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
    fmap f (Parser x) = Parser $ \s -> do   -- Make use of Maybe
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
          | Loop Char [Stmt] Char
data Program = Program [Stmt]
getStmts :: Program -> [Stmt]
getStmts (Program stmts) = stmts

stmtsToStr :: [Stmt] -> Int -> String
instance Show Stmt where
    show (MemOp x) = "MemOp " ++ show x
    show (PtrOp x) = "PtrOp " ++ show x
    show (IoOp x) = "IoOp " ++ show x
    show (Loop lb stmts rb) =
        "Loop [\n" ++ (stmtsToStr stmts 1) ++"\n]"
stmtsToStr [] _ = ""
stmtsToStr ((Loop _ substmts _):stmts) indent =
    let tabs = foldl1 (++) (take indent $ repeat "\t") in
    tabs ++ "Loop [\n" ++ (stmtsToStr substmts (indent + 1))
        ++ tabs ++ "]"
stmtsToStr (stmt:stmts) indent =
    let tabs = foldl1 (++) (take indent $ repeat "\t") in
    tabs ++ (show stmt) ++ "\n" ++ stmtsToStr stmts indent

instance Show Program where
    show (Program []) = ""
    show (Program (x:xs)) = show x ++ "\n" ++ (show $ Program xs)

char :: Char -> Parser Char
skip :: Parser Char
skipMany :: Parser [Char]
memOp :: Parser Stmt
ptrOp :: Parser Stmt
ioOp :: Parser Stmt
loop :: Parser Stmt
stmt :: Parser Stmt
program :: Parser Program

char c = Parser parser
    where
        parser [] = Nothing
        parser (x:xs)
            | x == c    = Just (c, xs)
            | otherwise = Nothing
skip = Parser parser
    where
        parser [] = Nothing
        parser (x:xs)
            | not (x `elem` "+-<>.,[]") = Just (x, xs)
            | otherwise = Nothing
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
