module Main (main) where

import Expr
  ( Expr(..)
  , Env
  , countResults
  , eval
  , evalBatch
  , prettyExpr
  , showResult
  , simplify
  )

main :: IO ()
main = do
  putStrLn "Haskell Expression Interpreter"
  mapM_ (display env) expressions
  putStrLn ""
  putStrLn ("Successful results: " ++ show (evalBatch env expressions))
  putStrLn ("Success/failure counts: " ++ show (countResults env expressions))
  putStrLn ("Simplified identity: " ++ prettyExpr (simplify identity))
  where
    env :: Env
    env = [("x", 3), ("y", 4)]
    expressions =
      [ Mul (Add (Var "x") (Var "y")) (Lit 2)
      , Let "x" (Lit 10) (Add (Var "x") (Var "y"))
      , Div (Lit 1) (Lit 0)
      , Var "missing"
      ]
    identity = Add (Lit 0) (Mul (Var "x") (Lit 1))

display :: Env -> Expr -> IO ()
display env expression =
  putStrLn (prettyExpr expression ++ "  " ++ showResult (eval env expression))
