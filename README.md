# hs-coexp

[![ci](https://github.com/vikraman/hs-coexp/actions/workflows/ci.yml/badge.svg)](https://github.com/vikraman/hs-coexp)

This is a Haskell library which explores some ideas around continuations and co-exponentials.

## Modules

The library has the following modules:

- `Control.Monad.Coexp`: a monadic interface for coexponentials
- `Control.Monad.Control`: a monadic interface for control operators using coexponentials
- `Data.Coexp`: implementation of coexponential types using `Cont`
- `Data.Coexp.Backtrack`: backtracking combinators
- `Control.Monad.Free.Control`: codensity encoding of `Free` monads using coexponentials
- `Control.Arrow.Coapply`: coarrows
- `Data.Profunctor.Coclosed`: coclosed profunctors

The examples are implemented in the following modules.

- `Examples.SAT.Guess`: bruteforce SAT solver using continuations
- `Examples.SAT.Backtrack`: backtracking SAT solver using the coexponential combinators
- `Examples.Eff.Toss`: Sam Lindley's drunken toss example of effect handlers encoded using codensity
- `Examples.TS.Backtrack`: backtracking tree search using the coexponential combinators

## Usage

There is a test suite which:

- _benchmarks_ the brute-force SAT solver against the backtracking SAT solver, and
- runs some test cases on the drunken toss example,
- tests the tree search example.

``` sh
$ cabal build all
$ cabal run coexp-test -- --quickcheck-tests 1000
```

## Comments

Any contributions and suggestions are most welcome.

Here are some other repositories (that I know of), which also explore continuations and co-exponentials:

- https://github.com/mstewartgallus/prologish/
- https://github.com/fresnel/fresnel
- https://github.com/robrix/sequoia
- https://github.com/jmanuel1/idris-stuff/
