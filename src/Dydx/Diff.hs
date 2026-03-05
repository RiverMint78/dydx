module Dydx.Diff (diff) where

import Dydx.Expr

diff :: (Num a) => String -> Expr a -> Expr a
diff _ NaN = NaN
diff _ (Const _) = Const 0
diff var (Var v)
    | v == var = Const 1
    | otherwise = Const 0
diff var (Add le re) = Add (diff var le) (diff var re)
diff var (Mul le re) = Add (Mul (diff var le) re) (Mul le (diff var re))
diff var (Pow u v) = Mul (Pow u v) (Add (Mul (Mul v (diff var u)) (Pow u (Const (-1)))) (Mul (diff var v) (Log u)))
diff var (Log u) = Mul (diff var u) (Pow u (Const (-1)))
diff var (Sin e) = Mul (Cos e) (diff var e)
diff var (Cos e) = Mul (neg (Sin e)) (diff var e)
diff var (Exp e) = Mul (Exp e) (diff var e)
