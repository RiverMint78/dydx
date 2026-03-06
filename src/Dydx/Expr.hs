module Dydx.Expr where

-- Expression Tree
data Expr a
  = Const !a
  | Var !String
  | Add !(Expr a) !(Expr a)
  | Mul !(Expr a) !(Expr a)
  | Pow !(Expr a) !(Expr a)
  | Log !(Expr a)
  | Exp !(Expr a)
  | Sin !(Expr a)
  | Cos !(Expr a)
  | NaN
  deriving (Show, Eq, Ord)

-- Constructors
neg :: (Num a) => Expr a -> Expr a
neg = Mul (Const (-1))
sub :: (Num a) => Expr a -> Expr a -> Expr a
sub le re = Add le (neg re)
divide :: (Num a) => Expr a -> Expr a -> Expr a
divide le re = Mul le (Pow re (Const (-1)))