# Dydx

[![Auto Release Windows Executable](https://github.com/RiverMint78/dydx/actions/workflows/release.yml/badge.svg)](https://github.com/RiverMint78/dydx/actions/workflows/release.yml)
![Haskell](https://img.shields.io/badge/Haskell-9.12.2-blueviolet?style=flat-square)

A minimal symbolic computation engine. Use it as a Haskell library or play with the interactive REPL.

> [!NOTE]
> This is a **toy project** for simple symbolic math.

## REPL Usage

Download the `.exe` from [Releases](https://github.com/RiverMint78/dydx/releases) and start playing:

```bash
dydx> diff x (x ^ 2 + 3 * x + 5)
2*x + 3

dydx> diffN 2 x x^3
6*x

dydx> diffN 2 x sin(exp(x) + y^2) + z
-exp(x)^2*sin(y^2 + exp(x)) + exp(x)*cos(y^2 + exp(x))

dydx> simp (0 * x + 1 * y + (5 - 5))
y

dydx> ln(-1)
NaN
```

The REPL uses `Expr Integer` for constant values, while the engine itself is not.

## Library

Dydx leverages Haskell's typeclasses (`Num`, `Fractional`, `Floating`) to allow native expression building:

```haskell
import Dydx.Expr

-- Define an expression
myExpr :: Expr Double
myExpr = asin (x ** 2 + 1) where x = Var "x"

-- The library builds the AST automatically:
-- Asin (Add (Pow (Var "x") (Const 2.0)) (Const 1.0))
```
