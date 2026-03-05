module Dydx.Pretty (pretty) where

import Dydx.Expr

pretty :: (Show a, Ord a, Num a) => Expr a -> String
pretty = prettyPrec 0

showParens :: Bool -> String -> String
showParens True s = "(" ++ s ++ ")"
showParens False s = s

-- 0: OUTER
-- 1: Add / Sub
-- 2: Mul / Div
-- 3: Unary Minus
-- 4: Pow
prettyPrec :: (Show a, Ord a, Num a) => Int -> Expr a -> String
prettyPrec _ NaN = "NaN"
-- Leaf
prettyPrec _ (Const c) = show c
prettyPrec _ (Var v) = v
-- Functions
prettyPrec _ (Sin e) = "sin(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Cos e) = "cos(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Exp e) = "exp(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Log e) = "ln(" ++ prettyPrec 0 e ++ ")"
-- Sub / Add
prettyPrec p (Add le (Mul (Const c) re))
    | c == -1 = showParens (p > 1) $ prettyPrec 1 le ++ " - " ++ prettyPrec 2 re
    | c < 0 = showParens (p > 1) $ prettyPrec 1 le ++ " - " ++ prettyPrec 2 (Mul (Const (negate c)) re)
prettyPrec p (Add le re) =
    showParens (p > 1) $ prettyPrec 1 le ++ " + " ++ prettyPrec 1 re
prettyPrec p (Mul (Const 1) e) = prettyPrec p e
prettyPrec p (Mul e (Const 1)) = prettyPrec p e
-- Div / Mul
prettyPrec p (Mul le (Pow re (Const (-1)))) =
    showParens (p > 2) $ prettyPrec 2 le ++ " / " ++ prettyPrec 3 re
prettyPrec p (Mul (Const (-1)) e) =
    showParens (p > 3) $ "-" ++ prettyPrec 4 e
prettyPrec p (Mul le re) =
    showParens (p > 2) $ prettyPrec 2 le ++ " * " ++ prettyPrec 2 re
-- Pow
prettyPrec p (Pow le re) =
    showParens (p > 4) $ prettyPrec 5 le ++ " ^ " ++ prettyPrec 4 re