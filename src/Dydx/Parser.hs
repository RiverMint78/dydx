module Dydx.Parser (parseExpr) where

import Data.Functor.Identity (Identity)
import Dydx.Expr (Expr (..), divide, neg, sub, tangent)
import Text.Parsec (
    between,
    char,
    choice,
    digit,
    eof,
    letter,
    many1,
    oneOf,
    parse,
    skipMany,
    string,
    try,
    (<?>),
 )
import Text.Parsec.Expr (
    Assoc (AssocLeft, AssocRight),
    Operator (Infix, Prefix),
    buildExpressionParser,
 )
import Text.Parsec.String (Parser)

parseExpr :: (Num a) => String -> Either String (Expr a)
parseExpr input = case parse (whitespace >> exprParser <* eof) "" input of
    Left err -> Left (show err)
    Right ex -> Right ex

-- Tokens

whitespace :: Parser ()
whitespace = skipMany (oneOf " \t")

lexeme :: Parser b -> Parser b
lexeme = (<* whitespace)

number :: (Num a) => Parser (Expr a)
number = lexeme $ Const . fromInteger . read <$> many1 digit

variable :: (Num a) => Parser (Expr a)
variable = lexeme $ choice [Exp (Const 1) <$ string "e", NaN <$ string "NaN", Var <$> many1 letter]

functionCall :: (Num a) => Parser (Expr a)
functionCall = try $ do
    fname <- many1 letter
    whitespace
    case lookup fname funcTable of
        Just con -> con <$> between (lexeme (char '(')) (lexeme (char ')')) exprParser
        Nothing -> fail $ "Unknown function: " ++ fname
  where
    funcTable =
        [ ("sin", Sin)
        , ("cos", Cos)
        , ("tan", tangent)
        , ("asin", Asin)
        , ("acos", Acos)
        , ("atan", Atan)
        , ("sinh", Sinh)
        , ("cosh", Cosh)
        , ("asinh", Asinh)
        , ("acosh", Acosh)
        , ("atanh", Atanh)
        , ("exp", Exp)
        , ("ln", Log)
        , ("log", Log)
        , ("sqrt", Sqrt)
        ]

-- Combinator

term :: (Num a) => Parser (Expr a)
term =
    choice [functionCall, number, variable, between (lexeme (char '(')) (lexeme (char ')')) exprParser]
        <?> "simple expression (number, variable, or function)"

operatorTable :: (Num a) => [[Operator String () Identity (Expr a)]]
operatorTable =
    [ [Infix (Pow <$ lexeme (char '^')) AssocRight]
    , [Prefix (neg <$ lexeme (char '-'))]
    , [Infix (Mul <$ lexeme (char '*')) AssocLeft, Infix (divide <$ lexeme (char '/')) AssocLeft]
    , [Infix (Add <$ lexeme (char '+')) AssocLeft, Infix (sub <$ lexeme (char '-')) AssocLeft]
    ]

exprParser :: (Num a) => Parser (Expr a)
exprParser = buildExpressionParser operatorTable term