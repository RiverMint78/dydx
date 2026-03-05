module Dydx.Pretty (pretty) where

import Dydx.Expr

pretty :: (Show a) => Expr a -> String
pretty (Const c) = show c
pretty (Var v) = v
-- arity == 1
pretty (Neg e) = "-(" ++ pretty e ++ ")"
pretty (Sin e) = "sin(" ++ pretty e ++ ")"
pretty (Cos e) = "cos(" ++ pretty e ++ ")"
pretty (Exp e) = "exp(" ++ pretty e ++ ")"
-- arity == 2
pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Sub e1 e2) = "(" ++ pretty e1 ++ " - " ++ pretty e2 ++ ")"
pretty (Mul e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"