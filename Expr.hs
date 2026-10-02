-- ============================================================
-- EC 8206 -- Functional Programming
-- Project Assignment: Arithmetic Expression Interpreter
-- University of Ruhuna
-- ============================================================

{-# OPTIONS_GHC -Wall #-}

module Expr
  ( -- * Core types
    Expr(..)
  , Env
    -- * Evaluation
  , eval
    -- * Higher-order / batch
  , simplify
  , evalBatch
  , countResults
    -- * Pretty printing
  , prettyExpr
  , showResult
    -- * Test runner
  , runTests
  ) where

import Data.Maybe (mapMaybe)
import System.Exit (exitFailure)

-- ============================================================
-- PART A: Language Design
-- ============================================================

-- | Algebraic data type representing arithmetic expressions.
data Expr
  = Lit Double          -- numeric literal: e.g. Lit 5.0
  | Var String          -- variable reference: e.g. Var "x"
  | Add Expr Expr       -- addition
  | Sub Expr Expr       -- subtraction
  | Mul Expr Expr       -- multiplication
  | Div Expr Expr       -- division
  | Let String Expr Expr -- let binding (extension): Let "x" e1 e2
  deriving (Show, Eq)

-- | Environment: a list of (variable name, value) pairs.
type Env = [(String, Double)]


-- ============================================================
-- PART B: Evaluation
-- ============================================================

-- | Evaluate an expression in a given environment.
--   Returns Left errorMsg on failure, Right value on success.
--   Failure cases: division by zero, undefined variable.
eval :: Env -> Expr -> Either String Double
eval _   (Lit n)       = Right n
eval env (Var x)       =
  case lookup x env of
    Just v  -> Right v
    Nothing -> Left ("Undefined variable: " ++ x)
eval env (Add e1 e2)   = applyOp (+) env e1 e2
eval env (Sub e1 e2)   = applyOp (-) env e1 e2
eval env (Mul e1 e2)   = applyOp (*) env e1 e2
eval env (Div e1 e2)   = do
  v1 <- eval env e1
  v2 <- eval env e2
  if v2 == 0
    then Left "Division by zero"
    else Right (v1 / v2)
eval env (Let x e1 e2) = do
  v1 <- eval env e1
  eval ((x, v1) : env) e2

-- | Helper: apply a binary op, propagating Either errors.
applyOp :: (Double -> Double -> Double)
        -> Env -> Expr -> Expr -> Either String Double
applyOp op env e1 e2 = do
  v1 <- eval env e1
  v2 <- eval env e2
  return (op v1 v2)


-- ============================================================
-- PART C: Higher-Order Functions
-- ============================================================

-- | Simplify an expression by rewriting algebraic identities.
--   Implements eight algebraic rewrites. Subexpressions are
--   simplified before the enclosing rule is selected, so one call reaches
--   a fixed point for the identities below on finite expression trees.
--   These are algebraic rewrites, not a semantics-preserving optimizer:
--   multiplication by zero can hide errors and change NaN/Infinity results.
--   IEEE signed-zero details can also change when removing addition by zero.
simplify :: Expr -> Expr
simplify (Lit n)       = Lit n
simplify (Var x)       = Var x
simplify (Add e1 e2)   =
  case (simplify e1, simplify e2) of
    (e, Lit 0) -> e
    (Lit 0, e) -> e
    (s1, s2)   -> Add s1 s2
simplify (Sub e1 e2)   =
  case (simplify e1, simplify e2) of
    (e, Lit 0) -> e
    (s1, s2)   -> Sub s1 s2
simplify (Mul e1 e2)   =
  case (simplify e1, simplify e2) of
    -- Check identity rules first so (-0.0) * 1 retains its sign.
    (e, Lit 1) -> e
    (Lit 1, e) -> e
    (_, Lit 0) -> Lit 0
    (Lit 0, _) -> Lit 0
    (s1, s2)   -> Mul s1 s2
simplify (Div e1 e2)   =
  case (simplify e1, simplify e2) of
    (e, Lit 1) -> e
    (s1, s2)   -> Div s1 s2
simplify (Let x e1 e2) = Let x (simplify e1) (simplify e2)


-- | Evaluate a batch of expressions. Returns only the successful results.
--   map applies the partially applied function eval env to each expression.
--   toRight converts successes to Just and failures to Nothing;
--   mapMaybe keeps the successful values.
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env exprs = mapMaybe toRight (map (eval env) exprs)
  where
    toRight (Right v) = Just v
    toRight (Left  _) = Nothing


-- | Count successes and failures in a batch using foldr.
--   Returns (successCount, failureCount).
countResults :: Env -> [Expr] -> (Int, Int)
countResults env exprs = foldr tally (0, 0) (map (eval env) exprs)
  where
    tally (Right _) (s, f) = (s + 1, f)
    tally (Left  _) (s, f) = (s, f + 1)


-- ============================================================
-- UTILITY: Pretty Printer
-- ============================================================

prettyExpr :: Expr -> String
prettyExpr (Lit n)       = show n
prettyExpr (Var x)       = x
prettyExpr (Add e1 e2)   = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Sub e1 e2)   = "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
prettyExpr (Mul e1 e2)   = "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
prettyExpr (Div e1 e2)   = "(" ++ prettyExpr e1 ++ " / " ++ prettyExpr e2 ++ ")"
prettyExpr (Let x e1 e2) = "(let " ++ x ++ " = " ++ prettyExpr e1
                         ++ " in " ++ prettyExpr e2 ++ ")"

showResult :: Either String Double -> String
showResult (Right v) = "= " ++ show v
showResult (Left  e) = "ERROR: " ++ e


-- ============================================================
-- TEST SUITE  (run in GHCi: :load Expr.hs  then  runTests)
-- ============================================================

sampleEnv :: Env
sampleEnv = [("x", 3.0), ("y", 4.0), ("z", 0.0)]

-- | Run every check, then exit unsuccessfully if any check failed.
--   In GHCi this raises ExitFailure; batch invocation returns a nonzero status.
runTests :: IO ()
runTests = do
  putStrLn "EC8206 Expression Interpreter Tests"
  evaluations <- mapM runEvaluation evaluationCases
  rewrites <- mapM runRewrite simplificationCases
  printing <- fmap concat (mapM runPrinting printingCases)
  batches <- fmap concat (mapM runBatch batchCases)
  extras <- sequence
    [ check "evalBatch keeps successes in order"
        (evalBatch sampleEnv batch) [1, 3, 9]
    , check "countResults includes failures"
        (countResults sampleEnv batch) (3, 2)
    , check "empty batch" (evalBatch [] []) []
    , check "empty counts" (countResults [] []) (0, 0)
    , check "all failures batch" (evalBatch [] [Var "x", badDivision]) []
    , check "all failures counts"
        (countResults [] [Var "x", badDivision]) (0, 2)
    , check "pretty Let scope"
        (prettyExpr (Add (Let "x" (Lit 2) (Var "x")) (Var "x")))
        "((let x = 2.0 in x) + x)"
    , check "pretty nested Let"
        (prettyExpr (Let "x" (Lit 2) (Let "y" (Var "x") (Var "y"))))
        "(let x = 2.0 in (let y = x in y))"
    , check "algebraic zero rewrite can suppress errors"
        (eval [] (simplify (Mul (Var "missing") (Lit 0)))) (Right 0)
    , check "unsimplified zero product retains errors"
        (eval [] (Mul (Var "missing") (Lit 0)))
        (Left "Undefined variable: missing")
    , check "simplify is idempotent on generated finite trees"
        (map (simplify . simplify) trees) (map simplify trees)
    , check "NaN literal remains a successful floating-point value"
        (rightSatisfies isNaN (eval [] (Lit nan))) True
    , check "infinity literal remains a successful floating-point value"
        (rightSatisfies isInfinite (eval [] (Lit infinity))) True
    , check "infinity times zero evaluates to NaN"
        (rightSatisfies isNaN (eval [] (Mul (Lit infinity) (Lit 0)))) True
    , check "zero rewrite replaces infinity product with zero"
        (eval [] (simplify (Mul (Lit infinity) (Lit 0)))) (Right 0)
    , check "NaN times zero evaluates to NaN"
        (rightSatisfies isNaN (eval [] (Mul (Lit nan) (Lit 0)))) True
    , check "zero rewrite replaces NaN product with zero"
        (eval [] (simplify (Mul (Lit nan) (Lit 0)))) (Right 0)
    , check "negative zero plus positive zero loses its sign"
        (fmap isNegativeZero (eval [] signedZeroSum)) (Right False)
    , check "addition rewrite preserves the negative-zero operand"
        (rightSatisfies isNegativeZero (eval [] (simplify signedZeroSum))) True
    , check "negative one times zero has a negative sign"
        (rightSatisfies isNegativeZero (eval [] signedZeroProduct)) True
    , check "zero-product rewrite loses the negative sign"
        (fmap isNegativeZero (eval [] (simplify signedZeroProduct))) (Right False)
    , check "multiplication by one preserves negative zero"
        (rightSatisfies isNegativeZero
          (eval [] (simplify (Mul (Lit (-0.0)) (Lit 1))))) True
    , check "division by one preserves negative zero"
        (rightSatisfies isNegativeZero
          (eval [] (simplify (Div (Lit (-0.0)) (Lit 1))))) True
    , check "environment lookup chooses the nearest binding"
        (eval [("x", 2), ("x", 9)] (Var "x")) (Right 2)
    , check "Let is nonrecursive in an empty environment"
        (eval [] (Let "x" (Var "x") (Lit 1)))
        (Left "Undefined variable: x")
    , check "batch counts and retained values agree on generated trees"
        (countResults sampleEnv trees)
        (length (evalBatch sampleEnv trees),
         length trees - length (evalBatch sampleEnv trees))
    ]
  putStrLn $ "Generated trees checked: " ++ show (length trees)
  finishTests (evaluations ++ rewrites ++ printing ++ batches ++ extras)
  where
    runEvaluation (name, expression, expected) =
      check name (eval sampleEnv expression) expected
    runRewrite (name, expression, expected) =
      check ("simplify " ++ name) (simplify expression) expected
    runPrinting (name, value, expected) = sequence
      [ check ("pretty " ++ name) (prettyExpr (Lit value)) expected
      , check ("showResult " ++ name) (showResult (Right value)) ("= " ++ expected)
      ]
    runBatch (name, expressions, expectedResults, expectedValues, expectedCounts) =
      sequence
        [ check ("batch evaluations: " ++ name)
            (map (eval sampleEnv) expressions) expectedResults
        , check ("batch values: " ++ name)
            (evalBatch sampleEnv expressions) expectedValues
        , check ("batch counts: " ++ name)
            (countResults sampleEnv expressions) expectedCounts
        ]
    -- Literal expected strings catch formatting regressions independently of show.
    printingCases =
      [ ("NaN", nan, "NaN")
      , ("positive infinity", infinity, "Infinity")
      , ("negative infinity", negate infinity, "-Infinity")
      , ("positive zero", 0.0, "0.0")
      , ("negative zero", -0.0, "-0.0")
      ]
    -- Hand-calculated results: none of these expectations comes from eval,
    -- evalBatch, countResults, or another function under test.
    batchCases =
      [ ( "mixed signs, fractions, duplicates, and failures"
        , [ Lit (-2), Div (Lit 9) (Lit 2), badDivision, Lit 0
          , Add (Var "x") (Lit 1), Var "missing", Lit (-2)
          ]
        , [ Right (-2), Right 4.5, Left "Division by zero", Right 0
          , Right 4, Left "Undefined variable: missing", Right (-2)
          ]
        , [-2, 4.5, 0, 4, -2]
        , (5, 2)
        )
      , ( "nested binding values, shadowing, and scope isolation"
        , [ Let "x" (Let "y" (Lit 2) (Add (Var "y") (Lit 1)))
              (Add (Var "x") (Var "y"))
          , Let "x" (Lit 10) (Let "x" (Add (Var "x") (Lit 1)) (Var "x"))
          , Add (Let "x" (Lit 2) (Var "x")) (Var "x")
          , Var "x"
          ]
        , [Right 7, Right 11, Right 5, Right 3]
        , [7, 11, 5, 3]
        , (4, 0)
        )
      , ( "binding errors, error precedence, and recovery"
        , [ Let "x" (Var "missing") (Lit 1)
          , Let "x" (Lit 1) badDivision
          , Div (Var "missing") (Lit 0)
          , Lit 8
          ]
        , [ Left "Undefined variable: missing", Left "Division by zero"
          , Left "Undefined variable: missing", Right 8
          ]
        , [8]
        , (1, 3)
        )
      ]
    badDivision = Div (Lit 1) (Lit 0)
    batch = [Lit 1, badDivision, Var "x", Var "w", Lit 9]
    atoms = [Lit (-1), Lit 0, Lit 1, Lit 2, Var "x", Var "missing"]
    shallowTrees = atoms
      ++ [op a b | op <- [Add, Sub, Mul, Div], a <- atoms, b <- atoms]
      ++ [Let "x" a b | a <- atoms, b <- atoms]
    -- 12,210 deterministic cases (not necessarily distinct trees).
    -- Includes left- and right-nested operators and nested Let bindings
    -- in initializers and bodies, with distinct and shadowed names.
    trees = shallowTrees
      ++ [op a b | op <- [Add, Sub, Mul, Div], a <- shallowTrees, b <- atoms]
      ++ [Let "x" a b | a <- shallowTrees, b <- atoms]
      ++ [op a b | op <- [Add, Sub, Mul, Div], a <- atoms, b <- shallowTrees]
      ++ [Let "x" a b | a <- atoms, b <- shallowTrees]
      ++ [ Let outer (Let inner value (Add (Var inner) delta))
             (Add (Var outer) body)
         | (outer, inner) <- [("x", "y"), ("x", "x")]
         , value <- atoms, delta <- atoms, body <- atoms
         ]
      ++ [ Let outer value
             (Let inner (Add (Var outer) delta) (Add (Var inner) body))
         | (outer, inner) <- [("x", "y"), ("x", "x")]
         , value <- atoms, delta <- atoms, body <- atoms
         ]
    nan = 0 / 0
    infinity = 1 / 0
    signedZeroSum = Add (Lit (-0.0)) (Lit 0)
    signedZeroProduct = Mul (Lit (-1)) (Lit 0)

-- NaN is not equal to itself, and ordinary equality ignores zero's sign.
-- Use explicit predicates when checking these floating-point cases.
rightSatisfies :: (Double -> Bool) -> Either String Double -> Bool
rightSatisfies predicate (Right value) = predicate value
rightSatisfies _ (Left _) = False

-- The first ten cases correspond exactly to the report's sample table.
-- All cases use sampleEnv, including literal-only examples.
evaluationCases :: [(String, Expr, Either String Double)]
evaluationCases =
  [ ("1. literal", Lit 5, Right 5)
  , ("2. variable", Var "x", Right 3)
  , ("3. undefined variable", Var "w", Left "Undefined variable: w")
  , ("4. division by zero", Div (Lit 1) (Lit 0), Left "Division by zero")
  , ("5. zero via variable", Div (Lit 10) (Var "z"), Left "Division by zero")
  , ("6. compound arithmetic", Mul (Add (Var "x") (Var "y")) (Lit 2), Right 14)
  , ("7. Let binding", Let "a" (Lit 10) (Add (Var "a") (Var "x")), Right 13)
  , ("8. subtraction", Sub (Lit 10) (Lit 3), Right 7)
  , ("9. multiplication", Mul (Lit 6) (Lit 7), Right 42)
  , ("10. shadowing", Let "x" (Lit 2)
        (Let "x" (Lit 5) (Add (Var "x") (Lit 1))), Right 6)
  , ("successful division", Div (Lit 7) (Lit 2), Right 3.5)
  , ("computed zero", Div (Lit 1) (Sub (Var "x") (Lit 3)),
        Left "Division by zero")
  , ("negative zero", Div (Lit 1) (Lit (-0.0)), Left "Division by zero")
  , ("left error first", Add (Var "missing") (Div (Lit 1) (Lit 0)),
        Left "Undefined variable: missing")
  , ("right error propagates", Mul (Lit 2) (Var "missing"),
        Left "Undefined variable: missing")
  , ("Let initializer uses outer scope",
        Let "x" (Add (Var "x") (Lit 1)) (Var "x"), Right 4)
  , ("Let binding does not leak",
        Add (Let "x" (Lit 2) (Var "x")) (Var "x"), Right 5)
  , ("Let initializer error", Let "x" (Var "missing") (Lit 1),
        Left "Undefined variable: missing")
  , ("Let body error", Let "x" (Lit 1) (Var "missing"),
        Left "Undefined variable: missing")
  ]

simplificationCases :: [(String, Expr, Expr)]
simplificationCases =
  [ ("x + 0", Add (Var "x") (Lit 0), Var "x")
  , ("0 + x", Add (Lit 0) (Var "x"), Var "x")
  , ("x - 0", Sub (Var "x") (Lit 0), Var "x")
  , ("x * 0", Mul (Var "x") (Lit 0), Lit 0)
  , ("0 * x", Mul (Lit 0) (Var "x"), Lit 0)
  , ("x * 1", Mul (Var "x") (Lit 1), Var "x")
  , ("1 * x", Mul (Lit 1) (Var "x"), Var "x")
  , ("x / 1", Div (Var "x") (Lit 1), Var "x")
  , ("nested identities", Add (Lit 0) (Mul (Var "x") (Lit 1)), Var "x")
  , ("nested zero product", Add (Mul (Var "x") (Lit 0)) (Var "y"), Var "y")
  , ("Let children", Let "a" (Add (Lit 2) (Lit 0))
        (Mul (Var "a") (Lit 1)), Let "a" (Lit 2) (Var "a"))
  , ("division by zero retained", Div (Lit 1) (Lit 0), Div (Lit 1) (Lit 0))
  ]

check :: (Eq a, Show a) => String -> a -> a -> IO Bool
check name actual expected = do
  let passed = actual == expected
  putStrLn $ "[" ++ (if passed then "PASS" else "FAIL") ++ "] " ++ name
  if passed
    then pure ()
    else do
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual:   " ++ show actual
  pure passed

finishTests :: [Bool] -> IO ()
finishTests results = do
  let passed = length (filter id results)
  let failed = length results - passed
  putStrLn $ show passed ++ " passed; " ++ show failed ++ " failed."
  if failed == 0 then pure () else exitFailure
