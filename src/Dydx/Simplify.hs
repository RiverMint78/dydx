module Dydx.Simplify (simplify) where

import Data.List (sortBy)
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

-- Add helpers
baseAdd :: (Num a) => Expr a -> Expr a
baseAdd (Mul (Const _) e) = e
baseAdd (Const _) = Const 1
baseAdd e = e

flattenAdd :: Expr a -> [Expr a]
flattenAdd (Add le re) = flattenAdd le ++ flattenAdd re
flattenAdd e = [e]

extractAdd :: (Num a) => Expr a -> (a, Expr a)
extractAdd (Mul (Const c) e) = (c, e)
extractAdd e = (1, e)

mergeAddTerms :: (Eq a, Ord a, Num a) => [Expr a] -> [Expr a]
mergeAddTerms [] = []
mergeAddTerms [x] = [x]
-- Const folding
mergeAddTerms (Const a : Const b : xs) = mergeAddTerms (Const (a + b) : xs)
-- Combine neighbor terms
mergeAddTerms (x : y : xs) =
    let (cx, bx) = extractAdd x
        (cy, by) = extractAdd y
     in if bx == by
            then mergeAddTerms (simplifyMul (Const (cx + cy)) bx : xs)
            else x : mergeAddTerms (y : xs)

buildAdd :: (Eq a, Num a) => [Expr a] -> Expr a
buildAdd [] = Const 0
buildAdd [x] = x
buildAdd (x : xs) = Add x (buildAdd xs)

simplifyAdd :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyAdd NaN _ = NaN
simplifyAdd _ NaN = NaN
simplifyAdd le re =
    let terms = sortBy (\x y -> compare (baseAdd x) (baseAdd y)) (flattenAdd le ++ flattenAdd re)
        merged = filter (/= Const 0) (mergeAddTerms terms)
     in if null merged then Const 0 else buildAdd merged

-- Mul helpers
baseMul :: (Num a) => Expr a -> Expr a
baseMul (Pow b _) = b
baseMul (Const _) = Const 1
baseMul e = e

flattenMul :: Expr a -> [Expr a]
flattenMul (Mul le re) = flattenMul le ++ flattenMul re
flattenMul e = [e]

extractMul :: (Num a) => Expr a -> (Expr a, Expr a)
extractMul (Pow b e) = (b, e)
extractMul e = (e, Const 1)

mergeMulTerms :: (Eq a, Ord a, Num a) => [Expr a] -> [Expr a]
mergeMulTerms [] = []
mergeMulTerms [x] = [x]
-- Const folding
mergeMulTerms (Const a : Const b : xs) = mergeMulTerms (Const (a * b) : xs)
-- Pow combine
mergeMulTerms (x : y : xs) =
    let (bx, ex) = extractMul x
        (by, ey) = extractMul y
     in if bx == by
            then mergeMulTerms (simplifyPow bx (simplifyAdd ex ey) : xs)
            else x : mergeMulTerms (y : xs)

buildMul :: (Eq a, Num a) => [Expr a] -> Expr a
buildMul [] = Const 1
buildMul [x] = x
buildMul (x : xs) = Mul x (buildMul xs)

simplifyMul :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyMul NaN _ = NaN
simplifyMul _ NaN = NaN
simplifyMul le re =
    let allTerms = flattenMul le ++ flattenMul re
     in if Const 0 `elem` allTerms
            then Const 0
            else
                let terms = sortBy (\x y -> compare (baseMul x) (baseMul y)) allTerms
                    merged = filter (/= Const 1) (mergeMulTerms terms)
                 in if null merged then Const 1 else buildMul merged

simplifyPow :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyPow NaN _ = NaN
simplifyPow _ NaN = NaN
-- Const folding
simplifyPow (Const 0) (Const n) | n < 0 = NaN
simplifyPow (Const 0) (Const 0) = NaN
simplifyPow _ (Const 0) = Const 1
simplifyPow e (Const 1) = e
simplifyPow (Const 0) _ = Const 0
simplifyPow (Const 1) _ = Const 1
-- Pow combine
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