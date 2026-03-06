module Main where

import Dydx.Diff
import Dydx.Expr
import Dydx.HighorderSimplify
import Dydx.Pretty

main :: IO ()
main = do
    let x = Var "x" :: Expr Integer
    let expr = Pow (Pow 2 (x * 2)) $ -1
    let d1 = simplifyFixed (diff "x" expr)
    let d2 = simplifyFixed (diffN 99 "x" d1)

    putStrLn $ "f   x: " ++ pretty expr
    putStrLn $ "f'  x: " ++ pretty d1
    putStrLn $ "f'' x: " ++ pretty d2