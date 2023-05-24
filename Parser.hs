module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Data.Char
import Expr

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \s -> Just(x,s)
    mp >>= f =
        Parser $ \s ->
            case parse mp s of
                Nothing -> Nothing
                Just(x, s2) -> parse (f x) s2 
        

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
    empty = Parser $ \s -> Nothing
    p1 <|> p2 = Parser $ \s ->
        case parse p1 s of
            Nothing -> parse p2 s
            Just(x, s2) -> Just(x, s2)

--- type declaration over ---

-- TODO 2.1. parse a expression
failParser :: Parser a
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just(c, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just(x, xs) else Nothing

parser_variable :: Parser Expr
parser_variable = do
    x <- predicateParser (isAlpha)
    return $ Variable [x]

parser_application :: Parser Expr
parser_application = do
    -- first <- charParser '('
    -- case first of
    --     Just _ ->
    --         do
    --             x <- parser_variable <|> parser_function
    --             charParser ' '
    --             y <- parser_variable <|> parser_function
    --             charParser ')'
    --             return $ Application x y
                
    --     _ -> 
    --         do
    --             x <- parser_variable <|> parser_function
    --             charParser ' '
    --             y <- parser_variable <|> parser_function
    --             return $ Application x y
    charParser '('
    x <- parser_variable <|> parser_function <|> parser_application
    charParser ' '
    y <- parser_variable <|> parser_function <|> parser_application
    charParser ')'
    return $ Application x y

parser_function :: Parser Expr 
parser_function = do
    charParser '\\'
    x <- predicateParser (isAlpha)
    charParser '.'
    y <- parser_variable <|> parser_function <|> parser_application
    return $ Function [x] y

parse_expr :: String -> Expr
parse_expr s = case parse (parser_variable <|> parser_function <|> parser_application) s of
    Just (x, _) -> x
    Nothing -> Variable "a"


-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
