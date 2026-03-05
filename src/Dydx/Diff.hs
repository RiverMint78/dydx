module Dydx.Diff (diff) where

import Dydx.Expr

diff :: (Num a) => String -> Expr a -> Expr a
diff _ (Const _) = Const 0
diff var (Var v)
    | v == var = Const 1
    | otherwise = Const 0
diff var (Add e1 e2) = Add (diff var e1) (diff var e2)
diff var (Sub e1 e2) = Sub (diff var e1) (diff var e2)
diff var (Neg e) = Neg (diff var e)
diff var (Mul e1 e2) = Add (Mul (diff var e1) e2) (Mul e1 (diff var e2))
diff var (Sin e) = Mul (Cos e) (diff var e)
diff var (Cos e) = Mul (Neg (Sin e)) (diff var e)
diff var (Exp e) = Mul (Exp e) (diff var e)
