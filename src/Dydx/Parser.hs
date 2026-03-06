module Dydx.Parser (parseExpr) where

import Dydx.Expr

parseExpr :: (Num a) => String -> Either String (Expr a)
parseExpr _ = Left "Not Implemented"