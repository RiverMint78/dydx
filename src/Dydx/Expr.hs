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

-- instances
instance (Num a) => Num (Expr a) where
  (+) = Add
  (*) = Mul
  (-) = sub
  negate = neg
  fromInteger = Const . fromInteger
  abs = error "Abs not implemented for Expr"
  signum = error "Signum not implemented for Expr"

instance (Fractional a) => Fractional (Expr a) where
  (/) = divide
  fromRational = Const . fromRational

instance (Floating a) => Floating (Expr a) where
  pi = Const pi
  exp = Exp
  log = Log
  sin = Sin
  cos = Cos
  (**) = Pow
  asin = error "asin not implemented"
  acos = error "acos not implemented"
  atan = error "atan not implemented"
  sinh = error "sinh not implemented"
  cosh = error "cosh not implemented"
  asinh = error "asinh not implemented"
  acosh = error "acosh not implemented"
  atanh = error "atanh not implemented"