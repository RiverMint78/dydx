module Main where

import Dydx.Diff
import Dydx.Expr
import Dydx.HighorderSimplify
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
    assertEq "NaN + x = NaN" NaN (simplify (Add NaN x))
    assertEq "5 * NaN = NaN" NaN (simplify (Mul (Const 5 :: Expr Int) NaN))
    assertEq "sin(NaN) = NaN" NaN (simplify (Sin NaN :: Expr Int))
    assertEq "exp(NaN) = NaN" NaN (simplify (Exp NaN :: Expr Int))

    -- / 0
    assertEq "1 / 0 = NaN" NaN (simplify (divide (Const 1 :: Expr Int) (Const 0)))
    assertEq "0 ^ 0 = NaN" NaN (simplify (Pow (Const 0 :: Expr Int) (Const 0)))
    assertEq "0 ^ -2 = NaN" NaN (simplify (Pow (Const 0 :: Expr Int) (Const (-2))))

    -- Log NaN
    assertEq "ln(0) = NaN" NaN (simplify (Log (Const 0 :: Expr Int)))
    assertEq "ln(-5) = NaN" NaN (simplify (Log (Const (-5) :: Expr Int)))

    -- sin(ln(0)) -> sin(NaN) -> NaN
    assertEq "sin(ln(0)) = NaN" NaN (simplify (Sin (Log (Const 0) :: Expr Int)))

    -- diff
    assertEq "diff(ln(0)) = NaN" NaN (simplify (diff "x" (Log (Const 0) :: Expr Int)))
    assertEq "diff(1/0) = NaN" NaN (simplify (diff "x" (divide (Const 1) (Const 0) :: Expr Int)))

main :: IO ()
main = do
    putStrLn "Running Diff tests..."

    assertEq "Const derivative" (Const 0 :: Expr Integer) (diff "x" (Const 5))
    assertEq "Var derivative" (Const 1 :: Expr Integer) (diff "x" (Var "x"))

    let x = Var "x" :: Expr Integer

    -- Add/Sub
    assertEq "Add derivative" (Add (Const 1) (Const 0)) (diff "x" (Add x (Const 5)))
    assertEq "Sub derivative (simplified)" (Const 1 :: Expr Integer) (simplify $ diff "x" (sub (Var "x") (Const 5)))

    -- Mul
    let mulExpr = Mul x x
    let expectedMul = Add (Mul (Const 1) x) (Mul x (Const 1))
    assertEq "Mul derivative" expectedMul (diff "x" mulExpr)

    -- Power Rule (x^2)
    let x2 = Pow x (Const 2)
    let expectedX2Diff = Mul (Pow x (Const 2)) (Add (Mul (Mul (Const 2) (Const 1)) (Pow x (Const (-1)))) (Mul (Const 0) (Log x)))
    assertEq "Pow derivative (Raw x^2)" expectedX2Diff (diff "x" x2)

    putStrLn "\nRunning Simplify tests..."
    assertEq "Simplify x - x" 0 (simplify (x - x)) -- instance of Num
    assertEq "Simplify 0 + x" x (simplify (0 + x))
    assertEq "Simplify x * 1" x (simplify (Mul x (Const 1)))
    assertEq "Simplify -(-x)" x (simplify (neg (neg x)))
    assertEq "Simplify x^1" x (simplify (Pow x (Const 1)))
    assertEq "Simplify x^0" (Const 1) (simplify (Pow x (Const 0)))

    putStrLn "\nRunning Integration (Diff + Simplify) tests..."

    assertEq "Simplify diff(x^2)" (Mul (Const 2) x) (simplify (diff "x" (Pow x (Const 2))))
    assertEq "Simplify diff(x^3)" (Mul (Const 3) (Pow x (Const 2))) (simplify (diff "x" (Pow x (Const 3))))
    assertEq "Simplify diff(5x)" (Const 5) (simplify (diff "x" x * 5))
    assertEq "Simplify diff(x/5)" (Pow (Const 5) (Const (-1))) (simplify (diff "x" (divide x (Const 5))))

    putStrLn "\nRunning Pretty Printer (Sugar Recovery) tests..."

    assertEq "Pretty Neg" "-x" (pretty (neg x))
    assertEq "Pretty Sub" "x - 5" (pretty (sub x (Const 5)))
    assertEq "Pretty Div" "x / 5" (pretty (divide x (Const 5)))
    assertEq "Pretty Pow" "x ^ 2" (pretty (Pow x (Const 2)))

    let complexExpr = Mul x (Sin x)
    putStrLn $ "Function:   " ++ pretty complexExpr
    putStrLn $ "Order 1:    " ++ (pretty . simplify . diff "x") complexExpr
    putStrLn $ "Order 2:    " ++ (pretty . simplify . diff "x" . diff "x") complexExpr
    putStrLn $ "Order 3:    " ++ (pretty . simplify . diff "x" . diff "x" . diff "x") complexExpr

    let invX = divide (Const 1) x
    putStrLn $ "Function:   " ++ pretty invX
    putStrLn $ "Derivative: " ++ (pretty . simplify . diff "x") invX

    -- NaN Tests
    runNaNTests

    putStrLn "\nAll tests passed successfully!"