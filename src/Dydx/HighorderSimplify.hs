module Dydx.HighorderSimplify (simplify) where

import Data.List (sortBy)
import Data.Ord (comparing)
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

flattenOp :: (Expr a -> Maybe (Expr a, Expr a)) -> Expr a -> [Expr a]
flattenOp matchOp expr = go expr []
 where
  go e acc = case matchOp e of
    Just (l, r) -> go l (go r acc)
    Nothing -> e : acc

buildOp :: Expr a -> (Expr a -> Expr a -> Expr a) -> [Expr a] -> Expr a
buildOp emptyVal _ [] = emptyVal
buildOp _ op xs = foldr1 op xs

simplifyPipeline ::
  (Eq a, Ord a, Num a) =>
  Expr a -> -- identity
  Maybe (Expr a) -> -- absorbing, 0 * E = 0
  (Expr a -> [Expr a]) -> -- flatten
  (Expr a -> Expr a) -> -- base for sort
  ([Expr a] -> [Expr a]) -> -- merge
  ([Expr a] -> Expr a) -> -- rebuild
  Expr a -> -- left expr
  Expr a -> -- right expr
  Expr a
simplifyPipeline _ _ _ _ _ _ NaN _ = NaN
simplifyPipeline _ _ _ _ _ _ _ NaN = NaN
simplifyPipeline ident absorbing flat getBase merge build le re
  | Just z <- absorbing, z `elem` terms = z
  | otherwise = build merged
 where
  terms = flat le ++ flat re
  sorted = sortBy (comparing getBase) terms
  merged = filter (/= ident) (merge sorted)

-- Add helpers
baseAdd :: (Num a) => Expr a -> Expr a
baseAdd (Mul (Const _) e) = e
baseAdd (Const _) = Const 1
baseAdd e = e

extractAdd :: (Num a) => Expr a -> (a, Expr a)
extractAdd (Mul (Const c) e) = (c, e)
extractAdd e = (1, e)

mergeAddTerms :: (Eq a, Ord a, Num a) => [Expr a] -> [Expr a]
mergeAddTerms [] = []
mergeAddTerms [x] = [x]
mergeAddTerms (Const a : Const b : xs) = mergeAddTerms (Const (a + b) : xs)
mergeAddTerms (x : y : xs)
  | bx == by = mergeAddTerms (simplifyMul (Const (cx + cy)) bx : xs)
  | otherwise = x : mergeAddTerms (y : xs)
 where
  (cx, bx) = extractAdd x
  (cy, by) = extractAdd y

simplifyAdd :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyAdd = simplifyPipeline (Const 0) Nothing flattenA baseAdd mergeAddTerms buildA
 where
  flattenA = flattenOp (\case Add l r -> Just (l, r); _ -> Nothing)
  buildA = buildOp (Const 0) Add

-- Mul helpers
baseMul :: (Num a) => Expr a -> Expr a
baseMul (Pow b _) = b
baseMul (Const _) = Const 1
baseMul e = e

extractMul :: (Num a) => Expr a -> (Expr a, Expr a)
extractMul (Pow b e) = (b, e)
extractMul e = (e, Const 1)

mergeMulTerms :: (Eq a, Ord a, Num a) => [Expr a] -> [Expr a]
mergeMulTerms [] = []
mergeMulTerms [x] = [x]
mergeMulTerms (Const a : Const b : xs) = mergeMulTerms (Const (a * b) : xs)
mergeMulTerms (x : y : xs)
  | bx == by = mergeMulTerms (simplifyPow bx (simplifyAdd ex ey) : xs)
  | otherwise = x : mergeMulTerms (y : xs)
 where
  (bx, ex) = extractMul x
  (by, ey) = extractMul y

simplifyMul :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyMul = simplifyPipeline (Const 1) (Just (Const 0)) flattenM baseMul mergeMulTerms buildM
 where
  flattenM = flattenOp (\case Mul l r -> Just (l, r); _ -> Nothing)
  buildM = buildOp (Const 1) Mul

-- Functions
simplifyPow :: (Eq a, Ord a, Num a) => Expr a -> Expr a -> Expr a
simplifyPow NaN _ = NaN
simplifyPow _ NaN = NaN
simplifyPow (Const 0) (Const n) | n < 0 = NaN
simplifyPow (Const 0) (Const 0) = NaN
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