module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Dydx.Diff (diff, diffN)
import Dydx.Expr
import Dydx.Parser (parseExpr)
import Dydx.Pretty (pretty)
import Dydx.Simplify (simplifyFixed)

main :: IO ()
main = do
    putStrLn "\nDydx available commands:"
    putStrLn "  diff <var> <expr>      - Compute 1st derivative"
    putStrLn "  diffN <n> <var> <expr> - Compute N-th derivative"
    putStrLn "  simp <expr>            - Simplify expression"
    putStrLn "  :q, :quit or exit      - Quit the program"
    putStrLn "-----------------------------------------"
    repl

repl :: IO ()
repl = do
    putStr "dydx> "
    hFlush stdout
    input <- getLine
    case words input of
        [] -> repl
        [cmd] | cmd `elem` [":q", ":quit", "exit"] -> putStrLn "Bye!"
        ("diff" : var : rest) -> do
            eval (unwords rest) (simplifyFixed . diff var)
            repl
        ("diffN" : nStr : var : rest) -> do
            maybe
                (putStrLn "Error: N must be an integer.")
                (\n -> eval (unwords rest) (diffN n var))
                (readMaybe nStr)
            repl
        ("simp" : rest) -> do
            eval (unwords rest) simplifyFixed
            repl
        _ -> do
            eval input simplifyFixed
            repl

eval :: String -> (Expr Integer -> Expr Integer) -> IO ()
eval exprStr processFn =
    either
        (\err -> putStrLn $ "Parse error: " ++ err)
        (putStrLn . pretty . processFn)
        (parseExpr exprStr)