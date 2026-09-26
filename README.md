# Haskell Expression Interpreter

A small arithmetic expression interpreter written in Haskell. It evaluates expression trees with variables and local bindings, reports errors explicitly, simplifies algebraic identities, and processes batches using higher-order functions.

The implementation uses only GHC's `base` library. No extra Haskell packages, API keys, or services are required.

## Features

- Algebraic data type with literals, variables, addition, subtraction, multiplication, division, and `Let` bindings.
- Recursive evaluation with `Either String Double` for explicit error handling.
- Lexical shadowing and nonrecursive local bindings.
- Eight algebraic simplification rules.
- Batch evaluation with `map` and `mapMaybe`, and success/failure counting with `foldr`.
- Fully parenthesized expression printing.
- 57 automated checks, including bounded checks over 5,766 generated expression trees.

## Quick start

Install GHC using [GHCup](https://www.haskell.org/ghcup/), then open a terminal in this repository's root folder. The project has been tested locally with GHC **9.10.3**.

Check compilation and run the tests:

```sh
ghc -Wall -Werror -fno-code Expr.hs
ghci -v0 Expr.hs -e runTests
```

Expected final test output:

```text
57 passed; 0 failed.
```

The test command returns a nonzero exit status if a check fails.

Run the demonstration:

```sh
ghci -v0 Expr.hs examples/Main.hs -e Main.main
```

These commands work in PowerShell and POSIX shells when GHC is on `PATH`.

## Interactive example

Start GHCi:

```sh
ghci Expr.hs
```

Then enter:

```haskell
let env = [("x", 3.0), ("y", 4.0)]
eval env (Mul (Add (Var "x") (Var "y")) (Lit 2))
-- Right 14.0

eval env (Let "x" (Lit 10) (Add (Var "x") (Var "y")))
-- Right 14.0

eval env (Div (Lit 1) (Lit 0))
-- Left "Division by zero"

eval env (Var "missing")
-- Left "Undefined variable: missing"

simplify (Add (Lit 0) (Mul (Var "x") (Lit 1)))
-- Var "x"

let batch = [Lit 1, Var "x", Div (Lit 1) (Lit 0), Var "missing"]
evalBatch env batch
-- [1.0,3.0]
countResults env batch
-- (2,2)
```

Expressions are constructed as `Expr` values. There is no parser for strings such as `"x + 2"`.

## Core API

- `eval :: Env -> Expr -> Either String Double` evaluates an expression or returns an error.
- `simplify :: Expr -> Expr` recursively applies algebraic identities.
- `evalBatch :: Env -> [Expr] -> [Double]` retains successful results in their original order.
- `countResults :: Env -> [Expr] -> (Int, Int)` returns success and failure counts.
- `prettyExpr :: Expr -> String` displays a parenthesized expression.
- `showResult :: Either String Double -> String` formats a result for display.
- `runTests :: IO ()` runs the embedded test suite.

An environment is an association list: `type Env = [(String, Double)]`. Lookup uses the first matching binding.

## Evaluation and simplification semantics

Evaluation propagates errors from the left operand before considering the right operand. Division checks for both positive and negative zero. A `Let` initializer uses the outer environment; its body receives the new binding, which does not leak into surrounding expressions.

`simplify` is an **algebraic simplifier, not a behavior-preserving optimizer**:

- `missing * 0` becomes zero and can suppress an undefined-variable error.
- `Infinity * 0` and `NaN * 0` can simplify to zero.
- Zero-related rewrites can change the sign of floating-point zero.
- NaN and infinity are valid `Double` values; the evaluator does not reject them as domain errors.

These trade-offs are documented and tested. Do not simplify before evaluation if preserving every error and floating-point detail is required.

## Tests and automation

Tests cover every expression constructor, error propagation, shadowing, nonrecursive binding scope, all eight rewrite rules, nested simplification, batch operations, and selected pretty-printing and floating-point cases.

Two bounded checks examine 5,766 generated trees for simplifier idempotence and batch consistency. This is not exhaustive verification or a proof of correctness.

[The GitHub Actions workflow](.github/workflows/haskell.yml) checks compilation, runs all tests, and runs the demo on Linux and Windows with GHC 9.10.3. Its hosted result is available only after the repository is pushed and the workflow runs.

## Repository contents

- [Expr.hs](Expr.hs) — interpreter, simplifier, batch functions, and tests.
- [examples/Main.hs](examples/Main.hs) — runnable demonstration.
- [.github/workflows/haskell.yml](.github/workflows/haskell.yml) — automated checks.

The coursework report and supplied assignment brief are deliberately not included. They remain in the original local project folder.

## References

- Graham Hutton, *Programming in Haskell*, 2nd ed., Cambridge University Press, 2016.
- Simon Thompson, *Haskell: The Craft of Functional Programming*, 2nd ed., Addison-Wesley, 1999.
- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
- [Data.Either documentation](https://hackage.haskell.org/package/base/docs/Data-Either.html)
- [Data.Maybe documentation](https://hackage.haskell.org/package/base/docs/Data-Maybe.html)

## License

No license has been selected for this repository.
