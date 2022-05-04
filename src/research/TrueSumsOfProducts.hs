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

import Data.Kind(Type)

-------------------------------------------------------------------------------

main :: IO ()
main = do
        print "True Sums of Products!"

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

data NP :: (k -> Type) -> [k] -> Type where
        Nil  :: NP f '[]
        (:*) :: f x -> NP f xs -> NP f (x ': xs)

data NS :: (k -> Type) -> [k] -> Type where
        Z :: f x -> NS f (x ': xs)
        S :: NS f xs -> NS f (x ': xs)

type SOP (f :: k -> *) (xss :: [[k]]) = NS (NP f) xss

type POP (f :: k -> *) (xss :: [[k]]) = NP (NP f) xss

