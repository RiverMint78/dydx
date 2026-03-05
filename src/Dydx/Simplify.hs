module Dydx.Simplify (simplify) where

import Dydx.Expr

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify (Add le re) = simplifyAdd (simplify le) (simplify re)
simplify (Sub le re) = simplifySub (simplify le) (simplify re)
simplify (Mul le re) = simplifyMul (simplify le) (simplify re)
simplify (Neg e) = simplifyNeg (simplify e)
simplify (Sin e) = simplifySin (simplify e)
simplify (Cos e) = simplifyCos (simplify e)
simplify (Exp e) = simplifyExp (simplify e)
simplify (Div le re) = Div (simplify le) (simplify re)
simplify e = e -- Const / Var

simplifyAdd :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
-- Const folding
simplifyAdd (Const a) (Const b) = Const (a + b)
simplifyAdd (Const 0) e = e
simplifyAdd e (Const 0) = e
-- Right-associative rewriting
simplifyAdd (Add le re) e = simplifyAdd le (simplifyAdd re e)
-- Neg convertion
simplifyAdd le (Neg e) = simplifySub le e
simplifyAdd (Neg e) re = simplifySub re e
-- Deep combine
simplifyAdd le (Add rle rre) =
    let combined = simplifyAdd le rle
     in if combined /= Add le rle
            then simplifyAdd combined rre
            else Add le (Add rle rre)
simplifyAdd le (Sub rle rre) =
    let combined = simplifyAdd le rle
     in if combined /= Add le rle
            then simplifySub combined rre
            else Sub (Add le rle) rre
-- Combine neighbor like terms
simplifyAdd le re | le == re = simplifyMul (Const 2) le
simplifyAdd (Mul (Const a) le) re | le == re = simplifyMul (Const (a + 1)) le
simplifyAdd le (Mul (Const a) re) | le == re = simplifyMul (Const (a + 1)) le
simplifyAdd (Mul (Const a) le) (Mul (Const b) re) | le == re = simplifyMul (Const (a + b)) le
-- Fallback
simplifyAdd le re = Add le re

simplifyMul :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
-- Const folding
simplifyMul (Const a) (Const b) = Const (a * b)
simplifyMul (Const 0) _ = Const 0
simplifyMul _ (Const 0) = Const 0
simplifyMul (Const 1) e = e
simplifyMul e (Const 1) = e
-- Absorb negatives
simplifyMul (Const a) (Neg e) = simplifyMul (Const (-a)) e
simplifyMul (Neg e) (Const a) = simplifyMul (Const (-a)) e
simplifyMul le (Neg re) = simplifyNeg (simplifyMul le re)
-- Extract and combine constants using associativity
simplifyMul (Const a) (Mul (Const b) re) = simplifyMul (Const (a * b)) re
simplifyMul (Mul (Const a) le) (Const b) = simplifyMul (Const (a * b)) le
simplifyMul (Mul (Const a) le) (Mul (Const b) re) = simplifyMul (Const (a * b)) (simplifyMul le re)
-- Right-associative rewriting
simplifyMul (Mul le re) e = simplifyMul le (simplifyMul re e)
-- Fallback
simplifyMul le re = Mul le re

simplifySub :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
-- Const folding
simplifySub (Const a) (Const b) = Const (a - b)
simplifySub e (Const 0) = e
simplifySub (Const 0) e = simplifyNeg e
-- Subtracting terms
simplifySub e1 e2 | e1 == e2 = Const 0
-- Combine neighbor like terms
simplifySub e1 (Mul (Const a) e2) | e1 == e2 = simplifyMul (Const (1 - a)) e1
simplifySub (Mul (Const a) e1) (Mul (Const b) e2) | e1 == e2 = simplifyMul (Const (a - b)) e1
-- Fallback
simplifySub le re = Sub le re

simplifyNeg :: (Eq a, Num a) => Expr a -> Expr a
-- Const folding
simplifyNeg (Const a) = Const (-a)
-- Double negation
simplifyNeg (Neg e) = e
-- Fallback
simplifyNeg e = Neg e

simplifySin :: (Eq a, Num a) => Expr a -> Expr a
simplifySin (Const 0) = Const 0
simplifySin e = Sin e

simplifyCos :: (Eq a, Num a) => Expr a -> Expr a
simplifyCos (Const 0) = Const 1
simplifyCos e = Cos e

simplifyExp :: (Eq a, Num a) => Expr a -> Expr a
simplifyExp (Const 0) = Const 1
simplifyExp e = Exp e