module Dydx.Simplify (simplify) where

import Dydx.Expr

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify (Add e1 e2) = simplifyAdd (simplify e1) (simplify e2)
simplify (Sub e1 e2) = simplifySub (simplify e1) (simplify e2)
simplify (Mul e1 e2) = simplifyMul (simplify e1) (simplify e2)
simplify (Neg e) = simplifyNeg (simplify e)
simplify (Sin e) = Sin (simplify e)
simplify (Cos e) = Cos (simplify e)
simplify (Exp e) = Exp (simplify e)
simplify e = e -- Const / Var

simplifyAdd :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
simplifyAdd (Const a) (Const b) = Const (a + b)
simplifyAdd (Const 0) e = e
simplifyAdd e (Const 0) = e
simplifyAdd e1 e2 = Add e1 e2

simplifyMul :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
simplifyMul (Const a) (Const b) = Const (a * b)
simplifyMul (Const 0) _ = Const 0
simplifyMul _ (Const 0) = Const 0
simplifyMul (Const 1) e = e
simplifyMul e (Const 1) = e
simplifyMul e1 e2 = Mul e1 e2

simplifySub :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
simplifySub (Const a) (Const b) = Const (a - b)
simplifySub e (Const 0) = e
simplifySub (Const 0) e = Neg e
simplifySub e1 e2 = Sub e1 e2

simplifyNeg :: (Eq a, Num a) => Expr a -> Expr a
simplifyNeg (Const a) = Const (-a)
simplifyNeg (Neg e) = e
simplifyNeg e = Neg e