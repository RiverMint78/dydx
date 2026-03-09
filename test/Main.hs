module Main where

import Dydx.Diff
import Dydx.Expr
import Dydx.Simplify
import Dydx.Pretty
import System.Exit (exitFailure)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq msg expected actual
    | expected == actual = putStrLn $ " [OK] " ++ msg
    | otherwise = do
        putStrLn $ " [FAIL] " ++ msg
        putStrLn $ "   Expected: " ++ show expected
        putStrLn $ "   Actual:   " ++ show actual
        exitFailure

runNaNTests :: IO ()
runNaNTests = do
    putStrLn "\nRunning NaN & Error Detection tests..."
    let x = Var "x" :: Expr Int

    -- Propagation
    assertEq "NaN + x = NaN" NaN (simplifyFixed (Add NaN x))
    assertEq "5*NaN = NaN" NaN (simplifyFixed (Mul (Const 5 :: Expr Int) NaN))
    assertEq "sin(NaN) = NaN" NaN (simplifyFixed (Sin NaN :: Expr Int))
    assertEq "exp(NaN) = NaN" NaN (simplifyFixed (Exp NaN :: Expr Int))

    -- / 0
    assertEq "0^(-1) = NaN" NaN (simplifyFixed (divide (Const 1 :: Expr Int) (Const 0)))
    assertEq "0^0 = NaN" NaN (simplifyFixed (Pow (Const 0 :: Expr Int) (Const 0)))
    assertEq "0^(-2) = NaN" NaN (simplifyFixed (Pow (Const 0 :: Expr Int) (Const (-2))))

    -- Log NaN
    assertEq "ln(0) = NaN" NaN (simplifyFixed (Log (Const 0 :: Expr Int)))
    assertEq "ln(-5) = NaN" NaN (simplifyFixed (Log (Const (-5) :: Expr Int)))

    -- sin(ln(0)) -> sin(NaN) -> NaN
    assertEq "sin(ln(0)) = NaN" NaN (simplifyFixed (Sin (Log (Const 0) :: Expr Int)))

    -- diff
    assertEq "diff(ln(0)) = NaN" NaN (simplifyFixed (diff "x" (Log (Const 0) :: Expr Int)))
    assertEq "diff(1/0) = NaN" NaN (simplifyFixed (diff "x" (divide (Const 1) (Const 0) :: Expr Int)))

runMoreMathTests :: IO ()
runMoreMathTests = do
    putStrLn "\nRunning More Math (Sqrt, Inverse Trig, Hyperbolic) tests..."

    let x = Var "x" :: Expr Integer

    -- d/dx[sqrt(x)] = 1 * (2 * sqrt(x))^(-1) -> 化简后应去掉 1 *
    let expectedSqrtDiff = Pow (Mul (Const 2) (Sqrt x)) (Const (-1))
    assertEq "simplifyFixed diff(sqrt(x))" expectedSqrtDiff (simplifyFixed (diff "x" (Sqrt x)))

    -- d/dx[sinh(x)] = cosh(x) * 1 -> 化简后为 cosh(x)
    assertEq "simplifyFixed diff(sinh(x))" (Cosh x) (simplifyFixed (diff "x" (Sinh x)))
    -- d/dx[cosh(x)] = sinh(x) * 1 -> 化简后为 sinh(x)
    assertEq "simplifyFixed diff(cosh(x))" (Sinh x) (simplifyFixed (diff "x" (Cosh x)))

    -- d/dx[asin(x)] = 1 * sqrt(1 - x^2)^(-1) -> 化简去掉 1 *
    let expectedAsinDiff = Pow (Sqrt (sub (Const 1) (Pow x (Const 2)))) (Const (-1))
    assertEq "simplifyFixed diff(asin(x))" expectedAsinDiff (simplifyFixed (diff "x" (Asin x)))

    -- d/dx[atan(x)] = 1 * (1 + x^2)^(-1)
    let expectedAtanDiff = Pow (Add (Const 1) (Pow x (Const 2))) (Const (-1))
    assertEq "simplifyFixed diff(atan(x))" expectedAtanDiff (simplifyFixed (diff "x" (Atan x)))

    -- d/dx[asinh(x)] = 1 * sqrt(x^2 + 1)^(-1)
    let expectedAsinhDiff = Pow (Sqrt (Add (Const 1) (Pow (Var "x") (Const 2)))) (Const (-1))
    assertEq "simplifyFixed diff(asinh(x))" expectedAsinhDiff (simplifyFixed (diff "x" (Asinh x)))

    -- const
    assertEq "simplifyFixed sinh(0) = 0" (Const 0) (simplifyFixed (Sinh (Const 0 :: Expr Int)))
    assertEq "simplifyFixed cosh(0) = 1" (Const 1) (simplifyFixed (Cosh (Const 0 :: Expr Int)))
    assertEq "simplifyFixed sqrt(x^2) = x" x (simplifyFixed (Sqrt (Pow x (Const 2))))

    -- print
    putStrLn $ "\nSqrt:       " ++ pretty (Sqrt x)
    putStrLn $ "Asin:       " ++ pretty (Asin x)
    putStrLn $ "Cosh:       " ++ pretty (Cosh x)
    putStrLn $ "Asin Diff:  " ++ (pretty . simplifyFixed . diff "x") (Asin x)
    putStrLn $ "Atan Diff3: " ++ pretty (diffN 3 "x" (Atan x))

main :: IO ()
main = do
    putStrLn "Running Diff tests..."

    assertEq "Const derivative" (Const 0 :: Expr Integer) (diff "x" (Const 5))
    assertEq "Var derivative" (Const 1 :: Expr Integer) (diff "x" (Var "x"))

    let x = Var "x" :: Expr Integer

    -- Add/Sub
    assertEq "Add derivative" (Add (Const 1) (Const 0)) (diff "x" (Add x (Const 5)))
    assertEq "Sub derivative (simplified)" (Const 1 :: Expr Integer) (simplifyFixed $ diff "x" (sub (Var "x") (Const 5)))

    -- Mul
    let mulExpr = Mul x x
    let expectedMul = Add (Mul (Const 1) x) (Mul x (Const 1))
    assertEq "Mul derivative" expectedMul (diff "x" mulExpr)

    -- Power Rule (x^2)
    assertEq "Pow derivative (x^2) simplified" (Mul 2 x) (simplifyFixed $ diff "x" (Pow x 2))

    putStrLn "\nRunning simplifyFixed tests..."
    assertEq "simplifyFixed x - x" 0 (simplifyFixed (x - x)) -- instance of Num
    assertEq "simplifyFixed 0 + x" x (simplifyFixed (0 + x))
    assertEq "simplifyFixed x * 1" x (simplifyFixed (Mul x (Const 1)))
    assertEq "simplifyFixed -(-x)" x (simplifyFixed (neg (neg x)))
    assertEq "simplifyFixed x^1" x (simplifyFixed (Pow x (Const 1)))
    assertEq "simplifyFixed x^0" (Const 1) (simplifyFixed (Pow x (Const 0)))

    putStrLn "\nRunning Integration (Diff + simplifyFixed) tests..."

    assertEq "simplifyFixed diff(x^2)" (Mul (Const 2) x) (simplifyFixed (diff "x" (Pow x (Const 2))))
    assertEq "simplifyFixed diff(x^3)" (Mul (Const 3) (Pow x (Const 2))) (simplifyFixed (diff "x" (Pow x (Const 3))))
    assertEq "simplifyFixed diff(5x)" (Const 5) (simplifyFixed (diff "x" x * 5))
    assertEq "simplifyFixed diff(x/5)" (Pow (Const 5) (Const (-1))) (simplifyFixed (diff "x" (divide x (Const 5))))

    putStrLn "\nRunning Pretty Printer (Sugar Recovery) tests..."

    assertEq "Pretty Neg" "-x" (pretty (neg x))
    assertEq "Pretty Sub" "x - 5" (pretty (sub x (Const 5)))
    assertEq "Pretty Div" "x*5^(-1)" (pretty (divide x (Const 5)))
    assertEq "Pretty Pow" "x^2" (pretty (Pow x (Const 2)))

    let complexExpr = Mul x (Sin x)
    putStrLn $ "\nFunction:   " ++ pretty complexExpr
    putStrLn $ "Order 1:    " ++ (pretty . simplifyFixed . diff "x") complexExpr
    putStrLn $ "Order 2:    " ++ pretty (diffN 2 "x" complexExpr)
    putStrLn $ "Order 99:   " ++ pretty (simplifyFixed (diffN 99 "x" complexExpr))

    let invX = divide (Const 1) x
    putStrLn $ "Function:   " ++ (pretty . simplify) invX
    putStrLn $ "Derivative: " ++ (pretty . simplify . diff "x") invX

    -- NaN Tests
    runNaNTests

    -- Math Tests
    runMoreMathTests

    putStrLn "\nAll tests passed successfully!"