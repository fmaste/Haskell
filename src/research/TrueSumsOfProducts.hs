{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Kind(Type)

main :: IO ()
main = do
        print "True Sums of Products!"

newtype I (a::Type) = I {unI :: a}

newtype K (a::Type) (b::k) = K {unK :: a}

type family Code (a::Type) :: [[ Type ]]

-- Example
data Expr = Num Int | Add {left :: Expr, right :: Expr}

type instance Code Expr = '[ '[Int], '[Expr, Expr] ]

