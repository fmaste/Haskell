- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html

# GADT Notation

What is GADT notation first: When the [GADTSyntax extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt_syntax.html) is enabled, GHC allows you to declare an algebraic data type by giving the type signatures of constructors explicitly.

For example:
```
{-# LANGUAGE GADTs #-}
data Maybe a =              |       data Maybe a where
        Nothing             |               Nothing :: Maybe a
        (Just a)            |               Just    :: a -> Maybe a
                            |
data Either a b =           |       data Either a b where {
          Left a            |               Left  :: a -> Either a b
        | Right b           |               Right :: b -> Either a b
        deriving( Eq, Ord ) |       } deriving( Eq, Ord )
                            |
newtype Down a =            |       newtype Down a where
        Down a              |               Down :: a -> Down a
```

Any datatype (or newtype) that can be declared in standard Haskell 98 syntax, can also be declared using GADT-style syntax. The choice is largely stylistic.

## Class constraints

But GADT-style declarations differ in one important aspect: A constructor signature may mention type class constraints, which can differ for different constructors. They treat class constraints on the data constructors differently, and if the constructor is given a type-class context, that context is made available by pattern matching.

For example:
```haskell
data Set a where
        MkSet :: Eq a => [a] -> Set a

makeSet :: Eq a => [a] -> Set a
makeSet xs = MkSet (nub xs)

insert :: a -> Set a -> Set a
insert a (MkSet as) | a `elem` as = MkSet as
                    | otherwise   = MkSet (a:as)
```

### Wasn't this considered a misfeature???

- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/datatype_contexts.html ???

# "Fun with phantom types"

Phantom types is another name for GADT. Also called "guarded recursive data types” or “first-class phantom types”.

The old Haskell 98 syntax for data declarations always declares an ordinary data type. The result type of each constructor must begin with the type constructor being defined, but for a GADT, the arguments to the type constructor can be arbitrary [monotypes](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/let_generalisation.html). For example, in a ```Term t``` data type, the type of each constructor must end with ```Term t```, but the ```t``` need not be the same type variable.

The magic of the GADT syntax starts with the presence of data constructors whose result type is not just ```T a b```, or which include contexts.

This is possible using the syntax described above because, the type signature of each constructor is independent, and is implicitly universally quantified as usual. But in particular, the type variable(s) in the ```data T a where``` header have no scope, and different constructors may have different universally-quantified type variables.

Also:
- 
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html


## Example

Suppose you want to embed a programming language, say, a simple expression language in Haskell. Since you are a firm believer of static typing, you would like your [embedded language](doc/EDSL.md) to be statically typed, as well.

This requirement rules out a simple ```Term``` data type as this choice would allow us to freely mix terms of different types.

```haskell
data Term =
          Zero
        | Succ Expr
        | Pred Expr
        | IsZero Expr
        | If Expr Expr Expr

-- This type checks:
-- >> If (Succ 1) (IsZero (Succ Zero)) (Pred $ If Zero Zero Zero)
```

The next idea is to parameterize the ```Term``` type so that ```Term t``` comprises only terms of type ```t```. The different compartments of ```Term``` are then inhabited by declaring constructors of the appropriate types.

```haskell
data Term t =
      Zero (Term t)
      Succ (Term t)
      Pred (Term t)
      IsZero (Term t)
      If (Term t) (Term t) (Term t)
```

Unfortunately, the above signatures cannot be translated into a data declaration (Haskell’s linguistic construct for introducing constructors) to build a syntax tree structure. The reason is simply that all constructors of a data type must share the same result type, namely, the declared type on the left-hand side.

The ["Fun with phantom types"](http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf) paper proposes something like this:

```haskell
data Term t =
          Zero                               with t = Int
        | Succ (Term Int)                    with t = Int
        | Pred (Term Int)                    with t = Int
        | IsZero (Term Int)                  with t = Bool
        | If (Term Bool) (Term a) (Term a)   with t = a
```
Note that the ```with``` clause of the ```If``` constructor is not strictly necessary. We could have simply replaced ```a``` by ```t```. Its main purpose is to illustrate that the type equation may contain type variables that do not appear on the left-hand side of the declaration. These variables can be seen as being existentially quantified.

Generalised Algebraic Data Types can only be declared using the syntactic explained above.
That using Haskell's GADT notation it converted into something like this:

```haskell
data Term t where
        Zero :: Term Int -> Term Int
        Succ, pred :: Term Int -> Term Int -> Term Int
        Zero :: Term Int -> Term Bool
        If :: Term Bool -> Term a -> Term a -> Term a
```

# Typing

The key point about GADTs is that pattern matching causes type refinement. For example, in the right hand side of the equation

```haskell
eval :: Term a -> a
eval (Lit i) =  ...
```

The type a is refined to ```Int```. That’s the whole point! A precise specification of the type rules is beyond what this user manual aspires to, but the design closely follows that described in the paper [Simple unification-based type inference for GADTs, (ICFP 2006)](https://research.microsoft.com/%7Esimonpj/papers/gadt/).

# Further reading

- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html
- https://www.haskell.org/haskellwiki/GADT
- https://wiki.haskell.org/Generalised_algebraic_datatype
- https://www.microsoft.com/en-us/research/publication/simple-unification-based-type-inference-for-gadts/
- https://github.com/goldfirere/glambda


TODO: [Visible type application](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#visible-type-application)


```haskell
data Expr a =
          I Int
        | B Bool
        | Add (Expr a) (Expr a)
        | Mul (Expr a) (Expr a)
        | Eq  (Expr a) (Expr a)
```

jajaja/hahaha
https://wiki.haskell.org/GADTs_for_dummies


This does not type checks!:
```haskell
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

```haskell
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

Note that ```Expr a``` constructors do not contain a value ```a``` at all; that's why ```a``` is called a phantom type, it's just a dummy variable.
