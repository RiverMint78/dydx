module Dydx.Diff (diff) where

import Dydx.Expr

diff :: (Num a) => String -> Expr a -> Expr a
diff _ (Const _) = Const 0
diff var (Var v)
    | v == var = Const 1
    | otherwise = Const 0
diff var (Add le re) = Add (diff var le) (diff var re)
diff var (Sub le re) = Sub (diff var le) (diff var re)
diff var (Mul le re) = Add (Mul (diff var le) re) (Mul le (diff var re))
diff var (Div le re) = Div (Sub (Mul (diff var le) re) (Mul le (diff var re))) (Mul re re)
diff var (Neg e) = Neg (diff var e)
diff var (Sin e) = Mul (Cos e) (diff var e)
diff var (Cos e) = Mul (Neg (Sin e)) (diff var e)
diff var (Exp e) = Mul (Exp e) (diff var e)
