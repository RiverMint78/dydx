module Main where

import Dydx.Diff
import Dydx.Expr
import Dydx.Pretty
import Dydx.Simplify
import System.Exit (exitFailure)

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

    assertEq "Const derivative" (Const 0 :: Expr Int) (diff "x" (Const 5))
    assertEq "Var derivative" (Const 1 :: Expr Int) (diff "x" (Var "x"))

    let addExpr = Add (Var "x") (Const 5)
    assertEq "Add derivative" (Add (Const 1 :: Expr Int) (Const 0 :: Expr Int)) (diff "x" addExpr)

    let mulExpr = Mul (Var "x") (Var "x")
    let expectedMul = Add (Mul (Const 1 :: Expr Int) (Var "x")) (Mul (Var "x") (Const 1 :: Expr Int))
    assertEq "Mul derivative" expectedMul (diff "x" mulExpr)

    let divExpr = Div (Var "x") (Const 5 :: Expr Int)
    let expectedDivDiff =
            Div
                (Sub (Mul (Const 1) (Const 5)) (Mul (Var "x") (Const 0)))
                (Mul (Const 5) (Const 5))
    assertEq "Div derivative (Raw)" expectedDivDiff (diff "x" divExpr)

    putStrLn "\nRunning Simplify tests..."
    assertEq "Simplify constant folding" (Const 6 :: Expr Int) (simplify (Mul (Const 2) (Const 3)))
    assertEq "Simplify 0 + x" (Var "x" :: Expr Int) (simplify (Add (Const 0) (Var "x")))
    assertEq "Simplify x * 1" (Var "x" :: Expr Int) (simplify (Mul (Var "x") (Const 1)))
    assertEq "Simplify -(-x)" (Var "x" :: Expr Int) (simplify (Neg (Neg (Var "x"))))

    putStrLn "\nRunning Integration (Diff + Simplify) tests..."

    assertEq
        "Simplify diff(x * x)"
        (Mul (Const 2) (Var "x") :: Expr Int)
        (simplify (diff "x" (Mul (Var "x") (Var "x"))))

    let fiveX = Mul (Const 5) (Var "x")
    assertEq
        "Simplify diff(5 * x)"
        (Const 5 :: Expr Int)
        (simplify (diff "x" fiveX))

    let expectedDivSimp = Div (Const 5 :: Expr Int) (Const 25 :: Expr Int)
    assertEq "Div derivative (Simplified)" expectedDivSimp (simplify (diff "x" divExpr))

    let invExpr = Div (Const 1 :: Expr Int) (Var "x")
    let expectedInvSimp = Div (Const (-1) :: Expr Int) (Mul (Var "x") (Var "x"))
    assertEq "Inverse derivative (Simplified)" expectedInvSimp (simplify (diff "x" invExpr))

    -- Pretty Test
    putStrLn "\nRunning Pretty Printer tests..."

    let complexExpr = diff "x" (Mul (Sin (Var "x")) (Var "x")) :: Expr Int
    putStrLn $ "Raw AST:    " ++ show complexExpr
    putStrLn $ "Pretty:     " ++ pretty complexExpr
    putStrLn $ "Simplified: " ++ pretty (simplify complexExpr)
    putStrLn $ "Order2:     " ++ (pretty . simplify . diff "x" . diff "x") complexExpr
    putStrLn $ "Order3:     " ++ (pretty . simplify . diff "x" . diff "x" . diff "x") complexExpr

    putStrLn "\nAll tests passed successfully!"