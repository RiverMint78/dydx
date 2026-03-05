module Dydx.Pretty (pretty) where

import Dydx.Expr

-- pretty
pretty :: (Show a) => Expr a -> String
pretty = prettyPrec 0

-- showParens: 是否外加括号
showParens :: Bool -> String -> String
showParens True s = "(" ++ s ++ ")"
showParens False s = s

prettyPrec :: (Show a) => Int -> Expr a -> String
-- Leaf, no parentheses
prettyPrec _ (Const c) = show c
prettyPrec _ (Var v) = v
-- Functions / Neg
prettyPrec _ (Neg e) = "-(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Sin e) = "sin(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Cos e) = "cos(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Exp e) = "exp(" ++ prettyPrec 0 e ++ ")"
-- Add / Sub
prettyPrec p (Add e1 e2) =
    showParens (p > 1) $ prettyPrec 1 e1 ++ " + " ++ prettyPrec 1 e2
prettyPrec p (Sub e1 e2) =
    -- x - (y - z)
    showParens (p > 1) $ prettyPrec 1 e1 ++ " - " ++ prettyPrec 2 e2
-- Mul
prettyPrec p (Mul e1 e2) =
    showParens (p > 2) $ prettyPrec 2 e1 ++ " * " ++ prettyPrec 2 e2