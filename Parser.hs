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

-- TODO 2.1. parse a expression
charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
    case s of
        "" -> Nothing
        (x:xs) -> if x == c then Just(c, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
    case s of
        "" -> Nothing
        (x:xs) -> if p x then Just(x, xs) else Nothing

parser_macro :: Parser Expr
parser_macro = do
    charParser '$'
    x <- many(predicateParser (isAlpha))
    return $ Macro x

parser_variable :: Parser Expr
parser_variable = do
    x <- predicateParser (isAlpha)
    return $ Variable [x]

parse_application_with_parantheses :: Parser Expr
parse_application_with_parantheses = do
    charParser '('
    expr <- parser_application
    charParser ')'
    return expr

parser_application :: Parser Expr
parser_application = do
    x <- parser_variable <|> parser_function <|> parse_application_with_parantheses <|> parser_macro
    rest <- many (
        do
            charParser ' '
            parser_variable <|> parser_function <|> parse_application_with_parantheses <|> parser_macro)
    return $ foldl Application x rest

parser_function :: Parser Expr 
parser_function = do
    charParser '\\'
    x <- predicateParser (isAlpha)
    charParser '.'
    y <- parser_variable <|> parser_function <|> parse_application_with_parantheses <|> parser_macro
    return $ Function [x] y

parse_expr :: String -> Expr
parse_expr s = case parse (parse_application_with_parantheses <|> parser_application <|> parser_function <|> parser_variable <|> parser_macro) s of
    Just (x, _) -> x
    Nothing -> error ("error while parsing expression")


-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code s =
    case parse (parser_assign <|> parser_evaluate) s of
        Just (x, _) -> x
        Nothing -> error ("error while parsing code") 

parser_assign :: Parser Code
parser_assign = do
    x <- many(predicateParser (isAlpha))
    many(charParser ' ')
    charParser '='
    many(charParser ' ')
    y <- parser_variable <|> parser_function <|> parse_application_with_parantheses <|> parser_macro
    return $ Assign x y

parser_evaluate :: Parser Code
parser_evaluate = do
    x <- parse_application_with_parantheses <|> parser_application <|> parser_function <|> parser_variable <|> parser_macro
    return $ Evaluate x