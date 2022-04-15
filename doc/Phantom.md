- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html#existential-quantification

# ["Fun with phantom types"](http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf). Phantom types is another name of GADT

Suppose you want to embed a programming language, say, a simple expression language in Haskell. Since you are a firm believer of static typing, you would like your embedded language to be statically typed, as well.

This requirement rules out a simple Term data type as this choice would allow us to freely mix terms of different types. The next idea is to parameterize the Term type so that Term t comprises only terms of type t

```
zero :: Term Int
succ, pred :: Term Int -> Term Int
isZero :: Term Int -> Term Bool
ifThenElse :: ∀a . Term Bool -> Term a -> Term a -> Term a.
```

Unfortunately, the above signatures cannot be translated into a data declaration (Haskell’s linguistic construct for introducing constructors). The reason is simply that all constructors of a data type must share the same result type, namely, the declared type on the left-hand side.

The paper proposes something like this:

```
data Term t =
          Zero                               with t = Int
        | Succ (Term Int)                    with t = Int
        | Pred (Term Int)                    with t = Int
        | IsZero (Term Int)                  with t = Bool
        | If (Term Bool ) (Term a) (Term a)  with t = a
```

Like saying what the ouput type should be

That using Haskell's GADT notation it converted into something like this:

```
data Term t where
        Zero :: Term Int -> Term Int
        Succ, pred :: Term Int -> Term Int -> Term Int
        Zero :: Term Int -> Term Bool
        If :: Term Bool -> Term a -> Term a -> Term a
```

- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt_syntax.html
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html


```
data Expr a =
          I Int
        | B Bool
        | Add (Expr a) (Expr a)
        | Mul (Expr a) (Expr a)
        | Eq  (Expr a) (Expr a)
```

Note that an expression ```Expr a``` does not contain a value ```a``` at all; that's why a is called a phantom type, it's just a dummy variable.

jajaja/hahaha
https://wiki.haskell.org/GADTs_for_dummies


This does not type checks!:
```
data Expr a =
        Num Int |
        Add (Expr Int) (Expr Int) |
        Mul (Expr Int) (Expr Int)

evalExpr :: Expr a -> a
evalExpr (Num n) = n
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
```

Error:

```
fmaste@server:~$ ghci GADT.hs
GHCi, version 8.10.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( GADT.hs, interpreted )

GADT.hs:34:20: error:
    • Couldn't match expected type ‘a’ with actual type ‘Int’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          evalExpr :: forall a. Expr a -> a
        at GADT.hs:33:1-23
    • In the expression: n
      In an equation for ‘evalExpr’: evalExpr (Num n) = n
    • Relevant bindings include
        evalExpr :: Expr a -> a (bound at GADT.hs:34:1)
   |
34 | evalExpr (Num n) = n
   |                    ^
Failed, no modules loaded.
Prelude>
```

Compiles!

```
{-# LANGUAGE GADTs #-}

data Expr a where
        Num :: Int -> Expr Int
        Add :: (Expr Int) -> (Expr Int) -> (Expr Int)
        Mul :: (Expr Int) -> (Expr Int) -> (Expr Int)

evalExpr :: Expr a -> a
evalExpr (Num n) = n
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
```
