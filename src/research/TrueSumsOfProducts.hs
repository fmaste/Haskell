-- For GArities.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}
-- Kind polymorphism:
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-- The extension TypeInType is now deprecated: its sole effect is to switch on
-- PolyKinds (and hence KindSignatures) and DataKinds.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import Data.Kind( Type )
import Data.Proxy ( Proxy(Proxy), asProxyTypeOf )
import GHC.Generics

-------------------------------------------------------------------------------

main :: IO ()
main = do
        print "True Sums of Products!"

-- "Old" GArities example:
-------------------------------------------------------------------------------

{--
class Arities a where
        arities :: Proxy a -> [Int]
        default arities :: (Generic a, GArities (Rep a)) => Proxy a -> [Int]
        arities x = garities (Proxy (from x))
--}

-- Proxy is a type that holds no data, but has a phantom parameter of arbitrary
-- type (or even kind). Its use is to provide type information, even though
-- there is no value available of that type (or it may be too costly to create
-- one).
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Proxy.html

class GArities (a :: k -> *) where
        --garities :: forall a. Proxy a -> [Int]
        garities :: Proxy a -> [Int]

-- Empty data types: V1.
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Generics.html#g:7
instance GArities V1 where
        garities _ = []

-- Constructors without fields: U1.
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Generics.html#g:8
instance GArities U1 where
        garities _ = [0]

-- Individual fields of constructors: K1.
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Generics.html#g:4
{--
src/research/TrueSumsOfProducts.hs:51:10: error:
    • Illegal instance declaration for ‘GArities (K1 R a)’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    • In the instance declaration for ‘GArities (K1 R a)’
   |
51 | instance GArities (K1 R a) where
--}
instance GArities (K1 R a) where
        garities _ = [1]

-- Meta information: M1.
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Generics.html#g:5
instance GArities f => GArities (M1 i t f) where
        garities _ = garities (Proxy :: Proxy f)

{--
*Main> :t (:+:)
<interactive>:1:1: error:
    • Data constructor not in scope: :+:
    • Perhaps you meant ‘:*:’ (imported from GHC.Generics)
*Main> :k (:+:)
(:+:) :: (k -> *) -> (k -> *) -> k -> *
--}
instance (GArities f, GArities g) => GArities (f :+: g) where
        garities _ =    garities (Proxy :: Proxy f)
                     ++ garities (Proxy :: Proxy g)
{-- Without {-# LANGUAGE ScopedTypeVariables #-}:
https://www.microsoft.com/en-us/research/uploads/prod/2018/06/tyvars-in-pats-haskell18-final.pdf

src/research/TrueSumsOfProducts.hs:98:32: error:
    • Could not deduce (GArities a1) arising from a use of ‘garities’
      from the context: (GArities f, GArities g)
        bound by the instance declaration
        at src/research/TrueSumsOfProducts.hs:97:10-55
      The type variable ‘a1’ is ambiguous
      These potential instances exist:
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :*: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:97:10
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :+: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:87:10
        instance GArities (K1 R a)
          -- Defined at src/research/TrueSumsOfProducts.hs:76:10
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the expression: garities (Proxy :: Proxy f)
      In a pattern binding: [x] = garities (Proxy :: Proxy f)
      In the expression:
        let
          [x] = garities (Proxy :: Proxy f)
          [y] = garities (Proxy :: Proxy g)
        in [x + y]
   |
98 |         garities _ = let [x] = garities (Proxy :: Proxy f)
   |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/research/TrueSumsOfProducts.hs:99:32: error:
    • Could not deduce (GArities a0) arising from a use of ‘garities’
      from the context: (GArities f, GArities g)
        bound by the instance declaration
        at src/research/TrueSumsOfProducts.hs:97:10-55
      The type variable ‘a0’ is ambiguous
      These potential instances exist:
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :*: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:97:10
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :+: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:87:10
        instance GArities (K1 R a)
          -- Defined at src/research/TrueSumsOfProducts.hs:76:10
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the expression: garities (Proxy :: Proxy g)
      In a pattern binding: [y] = garities (Proxy :: Proxy g)
      In the expression:
        let
          [x] = garities (Proxy :: Proxy f)
          [y] = garities (Proxy :: Proxy g)
        in [x + y]
   |
99 |                          [y] = garities (Proxy :: Proxy g)
   |
--}

{--
*Main> :t (:*:)
(:*:) :: f p -> g p -> (:*:) f g p
*Main> :k (:*:)
(:*:) :: (k -> *) -> (k -> *) -> k -> *
--}
instance forall f g.(GArities f, GArities g) => GArities (f :*: g) where
        garities _ = let [x] = garities (Proxy :: Proxy f)
                         [y] = garities (Proxy :: Proxy g)
                     in [x + y]
{-- Without {-# LANGUAGE ScopedTypeVariables #-}:
https://www.microsoft.com/en-us/research/uploads/prod/2018/06/tyvars-in-pats-haskell18-final.pdf

src/research/TrueSumsOfProducts.hs:98:32: error:
    • Could not deduce (GArities a1) arising from a use of ‘garities’
      from the context: (GArities f, GArities g)
        bound by the instance declaration
        at src/research/TrueSumsOfProducts.hs:97:10-55
      The type variable ‘a1’ is ambiguous
      These potential instances exist:
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :*: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:97:10
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :+: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:87:10
        instance GArities (K1 R a)
          -- Defined at src/research/TrueSumsOfProducts.hs:76:10
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the expression: garities (Proxy :: Proxy f)
      In a pattern binding: [x] = garities (Proxy :: Proxy f)
      In the expression:
        let
          [x] = garities (Proxy :: Proxy f)
          [y] = garities (Proxy :: Proxy g)
        in [x + y]
   |
98 |         garities _ = let [x] = garities (Proxy :: Proxy f)
   |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/research/TrueSumsOfProducts.hs:99:32: error:
    • Could not deduce (GArities a0) arising from a use of ‘garities’
      from the context: (GArities f, GArities g)
        bound by the instance declaration
        at src/research/TrueSumsOfProducts.hs:97:10-55
      The type variable ‘a0’ is ambiguous
      These potential instances exist:
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :*: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:97:10
        instance forall k (f :: k -> *) (g :: k -> *).
                 (GArities f, GArities g) =>
                 GArities (f :+: g)
          -- Defined at src/research/TrueSumsOfProducts.hs:87:10
        instance GArities (K1 R a)
          -- Defined at src/research/TrueSumsOfProducts.hs:76:10
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the expression: garities (Proxy :: Proxy g)
      In a pattern binding: [y] = garities (Proxy :: Proxy g)
      In the expression:
        let
          [x] = garities (Proxy :: Proxy f)
          [y] = garities (Proxy :: Proxy g)
        in [x + y]
   |
99 |                          [y] = garities (Proxy :: Proxy g)
   |
--}

-------------------------------------------------------------------------------

newtype I (a::Type) = I {unI :: a}

newtype K (a::Type) (b::k) = K {unK :: a}

type family Code (a::Type) :: [[ Type ]]

-- Example
-------------------------------------------------------------------------------

data Expr = Num Int | Add {left :: Expr, right :: Expr}

type instance Code Expr = '[ '[Int], '[Expr, Expr] ]

-- Version 0 of NP
-------------------------------------------------------------------------------

-- Needs extensions TypeOperators and GADTs
{--
data NP :: [Type] -> Type where
        Nil  :: NP '[]
        (:*) :: x -> NP xs -> NP (x ': xs)
--}

-- Example:
-- *Main> :t True :* ('x' :* ((3::Int) :* Nil))
-- True :* ('x' :* ((3::Int) :* Nil)) :: NP '[Bool, Char, Int]

{-- When compiled with no kind-signature expression as, "data NP where":

> ghc src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:30:17: error:
    • Expected kind ‘[k0] -> *’, but ‘NP’ has kind ‘*’
    • In the type ‘NP '[]’
      In the definition of data constructor ‘Nil’
      In the data declaration for ‘NP’
   |
30 |         Nil  :: NP '[]
   |

As we are promoting the ```[]``` data constructor to a type and the kind of
```'[]``` is polymorphic

Prelude> :k '[]
'[] :: [a]
Prelude> :k '[Int]
'[Int] :: [*]

The kind-signature expression is forcing it to be a "list" of ```Type```
insteads some other polymorphic kind.

Means: Constructor NP has kind ```[Type] -> Type``` (or ```[*] -> *``` in Haskell notation)
But GHC infers ```*``` from constructor ```Nil``` ? It's just ambigiuous!
--}

{-- When compiled with ```[]``` instead of ```'[]```:

> ghc src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:44:20: error:
    • Expecting one more argument to ‘[]’
      Expected kind ‘[*]’, but ‘[]’ has kind ‘* -> *’
    • In the first argument of ‘NP’, namely ‘[]’
      In the type ‘NP []’
      In the definition of data constructor ‘Nil’
   |
44 |         Nil  :: NP []
   |
--}

-- Functor version of NP:
-------------------------------------------------------------------------------

-- Type n-ary products.
data NP :: (k -> Type) -> [k] -> Type where
        Nil  :: NP f '[]
        (:*) :: f x -> NP f xs -> NP f (x ': xs)

npExample :: NP I '[Bool, Char, Int]
npExample = (I True) :* ((I 'x') :* ((I 3) :* Nil))

-- Type n-ary sums.
data NS :: (k -> Type) -> [k] -> Type where
        Z :: f x -> NS f (x ': xs)
        S :: NS f xs -> NS f (x ': xs)

type SOP (f :: k -> *) (xss :: [[k]]) = NS (NP f) xss

type POP (f :: k -> *) (xss :: [[k]]) = NP (NP f) xss
