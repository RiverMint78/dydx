module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Data.Char (toLower)
import Dydx.Diff (diffN)
import Dydx.Expr
import Dydx.Parser (parseExpr)
import Dydx.Pretty (pretty)
import Dydx.Simplify (simplifyFixed)

main :: IO ()
main = do
    putStrLn "Available commands:"
    putStrLn "  diff [n] <var> <expr>  - Compute N-th derivative (default n=1)"
    putStrLn "  <expr>                 - Simplify expression"
    putStrLn "  :q, :quit or :exit     - Quit the program"
    putStrLn "-----------------------------------------------------"
    repl

repl :: IO ()
repl = do
    putStr "dydx> " >> hFlush stdout
    input <- getLine
    case words input of
        [] -> repl
        (rawCmd : args)
            | let cmd = map toLower rawCmd
            , cmd `elem` [":q", ":quit", ":exit"] ->
                putStrLn "Bye!"
            | map toLower rawCmd == "diff" -> handleDiff args >> repl
        _ -> eval input simplifyFixed >> repl
  where
    handleDiff [] = putStrLn "Error: diff requires a variable and an expression."
    handleDiff (nStr : var : e : es)
        | Just n <- readMaybe nStr =
            if n < 0
                then putStrLn "Error: Derivative order cannot be negative."
                else eval (unwords (e : es)) (diffN n var)
    handleDiff (var : e : es) =
        eval (unwords (e : es)) (diffN 1 var)
    handleDiff _ = putStrLn "Error: Missing expression."

eval :: String -> (Expr Integer -> Expr Integer) -> IO ()
eval exprStr processFn =
    either
        (\err -> putStrLn $ "Parse error: " ++ err)
        (putStrLn . pretty . processFn)
        (parseExpr exprStr)