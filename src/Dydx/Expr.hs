module Dydx.Expr where

-- 表达式树定义
data Expr a
  = Const a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Neg (Expr a)
  | Sin (Expr a)
  | Cos (Expr a)
  | Exp (Expr a)
  deriving (Show, Eq)