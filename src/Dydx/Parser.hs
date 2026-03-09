module Dydx.Parser (parseExpr) where

import Data.Functor.Identity (Identity)
import Dydx.Expr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

parseExpr :: (Num a) => String -> Either String (Expr a)
parseExpr input = case parse (whitespace >> exprParser <* eof) "" input of
    Left err -> Left (show err)
    Right ex -> Right ex

-- Tokens

whitespace :: Parser ()
whitespace = skipMany (char ' ' <|> char '\t')

lexeme :: Parser b -> Parser b
lexeme p = do
    x <- p
    whitespace
    return x

number :: (Num a) => Parser (Expr a)
number = lexeme $ do
    digits <- many1 digit
    return $ Const (fromInteger (read digits))

variable :: (Num a) => Parser (Expr a)
variable = lexeme $ do
    name <- many1 letter
    case name of
        "e" -> return (Exp (Const 1))
        _ -> return (Var name)

functionCall :: (Num a) => Parser (Expr a)
functionCall = try $ do
    name <- many1 letter
    whitespace
    arg <- between (lexeme (char '(')) (lexeme (char ')')) exprParser
    case name of
        "sin" -> return $ Sin arg
        "cos" -> return $ Cos arg
        "asin" -> return $ Asin arg
        "acos" -> return $ Acos arg
        "atan" -> return $ Atan arg
        "sinh" -> return $ Sinh arg
        "cosh" -> return $ Cosh arg
        "asinh" -> return $ Asinh arg
        "acosh" -> return $ Acosh arg
        "atanh" -> return $ Atanh arg
        "exp" -> return $ Exp arg
        "ln" -> return $ Log arg
        "sqrt" -> return $ Sqrt arg
        "NaN" -> return NaN
        _ -> fail $ "Unknown function: " ++ name

term :: (Num a) => Parser (Expr a)
term =
    functionCall
        <|> number
        <|> variable
        <|> between (lexeme (char '(')) (lexeme (char ')')) exprParser
        <?> "simple expression (number, variable, or function)"

operatorTable :: (Num a) => [[Operator String () Identity (Expr a)]]
operatorTable =
    [ [Infix (lexeme (char '^') >> return Pow) AssocRight]
    , [Prefix (lexeme (char '-') >> return neg)]
    , [Infix (lexeme (char '*') >> return Mul) AssocLeft, Infix (lexeme (char '/') >> return divide) AssocLeft]
    , [Infix (lexeme (char '+') >> return Add) AssocLeft, Infix (lexeme (char '-') >> return sub) AssocLeft]
    ]

exprParser :: (Num a) => Parser (Expr a)
exprParser = buildExpressionParser operatorTable term