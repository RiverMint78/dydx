module Dydx.Expr where

-- Expression Tree
data Expr a
  = Const !a
  | Var !String
  | Add !(Expr a) !(Expr a)
  | Mul !(Expr a) !(Expr a)
  | Pow !(Expr a) !(Expr a)
  | Sqrt !(Expr a)
  | Log !(Expr a)
  | Exp !(Expr a)
  | Sin !(Expr a)
  | Cos !(Expr a)
  | Asin !(Expr a)
  | Acos !(Expr a)
  | Atan !(Expr a)
  | Sinh !(Expr a)
  | Cosh !(Expr a)
  | Asinh !(Expr a)
  | Acosh !(Expr a)
  | Atanh !(Expr a)
  | NaN
  deriving (Show, Eq, Ord)

-- Constructors
neg :: (Num a) => Expr a -> Expr a
neg = Mul (Const (-1))
sub :: (Num a) => Expr a -> Expr a -> Expr a
sub le re = Add le (neg re)
divide :: (Num a) => Expr a -> Expr a -> Expr a
divide le re = Mul le (Pow re (Const (-1)))
tangent :: (Num a) => Expr a -> Expr a
tangent e = Sin e `divide` Cos e

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
  asin = Asin
  acos = Acos
  atan = Atan
  sinh = Sinh
  cosh = Cosh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh