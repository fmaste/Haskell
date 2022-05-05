module Main where

main :: IO ()
main = print "Hello!"

-- data List a = Nil | Cons a (List a)

data ListF a f = NilF | ConsF a f

data Recursive f = Fix (f (Recursive f))

type List a = Recursive (ListF a)

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons a xs = Fix (ConsF a xs)
