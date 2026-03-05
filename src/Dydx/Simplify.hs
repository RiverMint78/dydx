module Dydx.Simplify (simplify) where

import Dydx.Expr

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify (Add le re) = simplifyAdd (simplify le) (simplify re)
simplify (Sub le re) = simplifySub (simplify le) (simplify re)
simplify (Mul le re) = simplifyMul (simplify le) (simplify re)
simplify (Neg e) = simplifyNeg (simplify e)
simplify (Sin e) = Sin (simplify e)
simplify (Cos e) = Cos (simplify e)
simplify (Exp e) = Exp (simplify e)
simplify (Div le re) = Div (simplify le) (simplify re)
simplify e = e -- Const / Var

simplifyAdd :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
simplifyAdd (Const a) (Const b) = Const (a + b)
simplifyAdd (Const 0) e = e
simplifyAdd e (Const 0) = e
simplifyAdd le re = Add le re

simplifyMul :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
simplifyMul (Const a) (Const b) = Const (a * b)
simplifyMul (Const 0) _ = Const 0
simplifyMul _ (Const 0) = Const 0
simplifyMul (Const 1) e = e
simplifyMul e (Const 1) = e
simplifyMul le re = Mul le re

simplifySub :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
simplifySub (Const a) (Const b) = Const (a - b)
simplifySub e (Const 0) = e
simplifySub (Const 0) e = Neg e
simplifySub le re = Sub le re

simplifyNeg :: (Eq a, Num a) => Expr a -> Expr a
simplifyNeg (Const a) = Const (-a)
simplifyNeg (Neg e) = e
simplifyNeg e = Neg e