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
prettyPrec _ (Sin e) = "sin(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Cos e) = "cos(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Exp e) = "exp(" ++ prettyPrec 0 e ++ ")"
prettyPrec p (Neg e) = showParens (p > 3) $ "-" ++ prettyPrec 4 e
-- Add / Sub
prettyPrec p (Add le re) =
    showParens (p > 1) $ prettyPrec 1 le ++ " + " ++ prettyPrec 1 re
prettyPrec p (Sub le re) =
    -- x - (y - z)
    showParens (p > 1) $ prettyPrec 1 le ++ " - " ++ prettyPrec 2 re
-- Mul / Div
prettyPrec p (Mul le re) =
    showParens (p > 2) $ prettyPrec 2 le ++ " * " ++ prettyPrec 2 re
prettyPrec p (Div le re) =
    -- a / (b / c)
    showParens (p > 2) $ prettyPrec 2 le ++ " / " ++ prettyPrec 3 re