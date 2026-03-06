module Dydx.Pretty (pretty) where

import Dydx.Expr

pretty :: (Show a, Ord a, Num a) => Expr a -> String
pretty = prettyPrec 0

showParens :: Bool -> String -> String
showParens True s = "(" ++ s ++ ")"
showParens False s = s

stripNeg :: (Num a, Ord a) => Expr a -> Maybe (Expr a)
stripNeg (Const c) | c < 0 = Just (Const (negate c))
stripNeg (Mul (Const c) e)
    | c == -1 = Just e
    | c < 0 = Just (Mul (Const (negate c)) e)
stripNeg (Mul le re)
    | Just le' <- stripNeg le = Just (Mul le' re)
stripNeg _ = Nothing

-- 0: OUTER
-- 1: Add / Sub
-- 2: Mul
-- 2: Unary Minus
-- 4: Pow
prettyPrec :: (Show a, Ord a, Num a) => Int -> Expr a -> String
prettyPrec _ NaN = "NaN"
-- Functions
prettyPrec _ (Sin e) = "sin(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Cos e) = "cos(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Exp e) = "exp(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Log e) = "ln(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Sqrt e) = "sqrt(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Asin e) = "asin(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Acos e) = "acos(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Atan e) = "atan(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Sinh e) = "sinh(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Cosh e) = "cosh(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Asinh e) = "asinh(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Acosh e) = "acosh(" ++ prettyPrec 0 e ++ ")"
prettyPrec _ (Atanh e) = "atanh(" ++ prettyPrec 0 e ++ ")"
-- Sub / Add
prettyPrec p (Add le re) =
    showParens (p > 1) $ prettyPrec 1 le ++ renderTail re
  where
    renderTail (Add l r) = render l ++ renderTail r
    renderTail e = render e
    render t
        | Just t' <- stripNeg t = " - " ++ prettyPrec 2 t'
        | otherwise = " + " ++ prettyPrec 1 t
-- Unary Minus
prettyPrec p e | Just e' <- stripNeg e = showParens (p > 3) $ "-" ++ prettyPrec 2 e'
-- Mul
prettyPrec p (Mul (Const (-1)) e) = showParens (p > 3) $ "-" ++ prettyPrec 2 e
prettyPrec p (Mul le re) = showParens (p > 2) $ prettyPrec 2 le ++ "*" ++ prettyPrec 2 re
-- Pow
prettyPrec p (Pow le re) = showParens (p > 4) $ prettyPrec 5 le ++ "^" ++ prettyPrec 4 re
-- Leaf
prettyPrec _ (Const c) = show c
prettyPrec _ (Var v) = v