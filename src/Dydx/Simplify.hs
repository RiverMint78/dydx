module Dydx.Simplify (simplify) where

import Dydx.Expr

simplify :: (Eq a, Ord a, Num a) => Expr a -> Expr a
simplify NaN = NaN
simplify (Add le re) = simplifyAdd (simplify le) (simplify re)
simplify (Mul le re) = simplifyMul (simplify le) (simplify re)
simplify (Pow le re) = simplifyPow (simplify le) (simplify re)
simplify (Log e) = simplifyLog (simplify e)
simplify (Sin e) = simplifySin (simplify e)
simplify (Cos e) = simplifyCos (simplify e)
simplify (Exp e) = simplifyExp (simplify e)
simplify e = e -- Const / Var

simplifyAdd :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyAdd NaN _ = NaN
simplifyAdd _ NaN = NaN
-- Const folding
simplifyAdd (Const a) (Const b) = Const (a + b)
simplifyAdd (Const 0) e = e
simplifyAdd e (Const 0) = e
-- Const bubble-up
simplifyAdd e (Const a) = simplifyAdd (Const a) e
simplifyAdd (Const a) (Add (Const b) re) = simplifyAdd (Const (a + b)) re
-- Right-associative rewriting
simplifyAdd (Add le re) e = simplifyAdd le (simplifyAdd re e)
-- Combine neighbor terms
simplifyAdd le re | le == re = simplifyMul (Const 2) le
simplifyAdd (Mul (Const a) le) re | le == re = simplifyMul (Const (a + 1)) le
simplifyAdd le (Mul (Const a) re) | le == re = simplifyMul (Const (a + 1)) le
simplifyAdd (Mul (Const a) le) (Mul (Const b) re) | le == re = simplifyMul (Const (a + b)) le
-- Deep combine
simplifyAdd le (Add rle rre) =
    let combined = simplifyAdd le rle
     in if combined /= Add le rle then simplifyAdd combined rre else Add le (Add rle rre)
-- Fallback
simplifyAdd le re = Add le re

simplifyMul :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyMul NaN _ = NaN
simplifyMul _ NaN = NaN
-- Const folding
simplifyMul (Const a) (Const b) = Const (a * b)
simplifyMul (Const 0) _ = Const 0
simplifyMul _ (Const 0) = Const 0
simplifyMul (Const 1) e = e
simplifyMul e (Const 1) = e
-- Const bubble-up
simplifyMul e (Const a) = simplifyMul (Const a) e
simplifyMul (Const a) (Mul (Const b) re) = simplifyMul (Const (a * b)) re
simplifyMul le (Mul (Const a) re) = simplifyMul (Const a) (simplifyMul le re)
-- Right-associative rewriting
simplifyMul (Mul le re) e = simplifyMul le (simplifyMul re e)
-- Pow combine
simplifyMul le re | le == re = simplifyPow le (Const 2)
simplifyMul (Pow u a) (Pow v b) | u == v = simplifyPow u (simplifyAdd a b)
simplifyMul le (Pow re b) | le == re = simplifyPow le (simplifyAdd (Const 1) b)
simplifyMul (Pow le a) re | le == re = simplifyPow le (simplifyAdd a (Const 1))
-- Deep combine
simplifyMul le (Mul rle rre) =
    let combined = simplifyMul le rle
     in if combined /= Mul le rle then simplifyMul combined rre else Mul le (Mul rle rre)
-- Fallback
simplifyMul le re = Mul le re

simplifyPow :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyPow NaN _ = NaN
simplifyPow _ NaN = NaN
simplifyPow (Const 0) (Const n) | n < 0 = NaN
simplifyPow (Const 0) (Const 0) = NaN
-- simplifyPow (Const a) (Const b) | b >= 0 = Const (a ^ b)
simplifyPow _ (Const 0) = Const 1
simplifyPow e (Const 1) = e
simplifyPow (Const 0) _ = Const 0
simplifyPow (Const 1) _ = Const 1
simplifyPow (Pow u a) b = simplifyPow u (simplifyMul a b)
simplifyPow le re = Pow le re

simplifyLog :: (Eq a, Ord a, Num a) => Expr a -> Expr a
simplifyLog NaN = NaN
simplifyLog (Const n) | n <= 0 = NaN
simplifyLog (Const 1) = Const 0
simplifyLog (Exp e) = e
simplifyLog e = Log e

simplifySin :: (Eq a, Num a) => Expr a -> Expr a
simplifySin NaN = NaN
simplifySin (Const 0) = Const 0
simplifySin e = Sin e

simplifyCos :: (Eq a, Num a) => Expr a -> Expr a
simplifyCos NaN = NaN
simplifyCos (Const 0) = Const 1
simplifyCos e = Cos e

simplifyExp :: (Eq a, Num a) => Expr a -> Expr a
simplifyExp NaN = NaN
simplifyExp (Const 0) = Const 1
simplifyExp (Log e) = e
simplifyExp e = Exp e