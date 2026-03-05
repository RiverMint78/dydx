module Main where

import Dydx.Diff
import Dydx.Expr
import Dydx.Simplify
import System.Exit (exitFailure)
import Dydx.Pretty

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq msg expected actual
    | expected == actual = putStrLn $ " [OK] " ++ msg
    | otherwise = do
        putStrLn $ " [FAIL] " ++ msg
        putStrLn $ "   Expected: " ++ show expected
        putStrLn $ "   Actual:   " ++ show actual
        exitFailure

main :: IO ()
main = do
    putStrLn "Running Diff tests..."

    -- d/dx(5.0) = 0.0
    assertEq "Const derivative" (Const 0 :: Expr Double) (diff "x" (Const 5))

    -- d/dx(x) = 1.0, double
    assertEq "Var derivative" (Const 1 :: Expr Double) (diff "x" (Var "x"))

    -- d/dx(x + 5) = 1 + 0
    let addExpr = Add (Var "x") (Const 5)
    assertEq "Add derivative" (Add (Const 1 :: Expr Int) (Const 0 :: Expr Int)) (diff "x" addExpr)

    -- d/dx(x * x) = (1 * x) + (x * 1)
    let mulExpr = Mul (Var "x") (Var "x")
    let expectedMul = Add (Mul (Const 1 :: Expr Int) (Var "x")) (Mul (Var "x") (Const 1 :: Expr Int))
    assertEq "Mul derivative" expectedMul (diff "x" mulExpr)

    putStrLn "\nRunning Simplify tests..."

    -- 常量折叠: 2 * 3 = 6
    assertEq
        "Simplify constant folding"
        (Const 6 :: Expr Int)
        (simplify (Mul (Const 2) (Const 3)))

    -- 代数化简: 0 + x = x
    assertEq
        "Simplify 0 + x"
        (Var "x" :: Expr Int)
        (simplify (Add (Const 0) (Var "x")))

    -- 代数化简: x * 1 = x
    assertEq
        "Simplify x * 1"
        (Var "x" :: Expr Int)
        (simplify (Mul (Var "x") (Const 1)))

    -- 代数化简: -(-x) = x
    assertEq
        "Simplify -(-x)"
        (Var "x" :: Expr Int)
        (simplify (Neg (Neg (Var "x"))))

    putStrLn "\nRunning Integration (Diff + Simplify) tests..."

    -- 综合测试: d/dx(x * x)  x + x
    assertEq
        "Simplify diff(x * x)"
        (Add (Var "x") (Var "x") :: Expr Int)
        (simplify (diff "x" (Mul (Var "x") (Var "x"))))

    -- 综合测试: d/dx(5 * x) 简化后应该是 5
    let fiveX = Mul (Const 5) (Var "x")
    assertEq
        "Simplify diff(5 * x)"
        (Const 5 :: Expr Int)
        (simplify (diff "x" fiveX))

    -- Pretty Test
    putStrLn "\nRunning Pretty Printer tests..."

    let complexExpr = diff "x" (Mul (Sin (Var "x")) (Var "x")) :: Expr Int
    putStrLn $ "Raw AST:    " ++ show complexExpr
    putStrLn $ "Pretty:     " ++ pretty complexExpr
    putStrLn $ "Simplified: " ++ pretty (simplify complexExpr)

    putStrLn "\nAll tests passed successfully!"