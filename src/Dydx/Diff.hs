module Dydx.Diff (diff, diffN) where

import Dydx.Expr
import Dydx.Simplify (simplifyFixed)

diff :: (Num a) => String -> Expr a -> Expr a
diff _ NaN = NaN
diff _ (Const _) = Const 0
diff var (Var v)
    | v == var = Const 1
    | otherwise = Const 0
diff var (Add le re) = diff var le + diff var re
diff var (Mul le re) = diff var le * re + le * diff var re
diff var (Pow u v) = Pow u v * (v * diff var u * Pow u (-1) + diff var v * Log u)
diff var (Sqrt u) = diff var u * Pow (2 * Sqrt u) (-1)
diff var (Log u) = diff var u * Pow u (-1)
diff var (Exp e) = Exp e * diff var e
diff var (Sin e) = Cos e * diff var e
diff var (Cos e) = -(Sin e * diff var e)
diff var (Asin u) = diff var u * Pow (Sqrt (1 - Pow u 2)) (-1)
diff var (Acos u) = -(diff var u * Pow (Sqrt (1 - Pow u 2)) (-1))
diff var (Atan u) = diff var u * Pow (1 + Pow u 2) (-1)
diff var (Sinh u) = Cosh u * diff var u
diff var (Cosh u) = Sinh u * diff var u
diff var (Asinh u) = diff var u * Pow (Sqrt (Pow u 2 + 1)) (-1)
diff var (Acosh u) = diff var u * Pow (Sqrt (Pow u 2 - 1)) (-1)
diff var (Atanh u) = diff var u * Pow (1 - Pow u 2) (-1)

diffN :: (Eq a, Ord a, Num a) => Int -> String -> Expr a -> Expr a
diffN n var = (!! n) . iterate (simplifyFixed . diff var)