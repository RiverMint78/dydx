module Main where

import Dydx.Expr
import Dydx.Diff
import Dydx.Simplify
import Dydx.Pretty

main :: IO ()
main = do
    let x = Var "x" :: Expr Integer 
    let expr = Mul x (Sin (Exp x))
    let d1 = simplify (diff "x" expr)
    let d2 = simplify (diff "x" d1)
    
    putStrLn $ "f   x: " ++ pretty expr
    putStrLn $ "f'  x: " ++ pretty d1
    putStrLn $ "f'' x: " ++ pretty d2