{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

main :: IO ()
main = do 
        print ((myAdd (1::Int) (3.0::Float))::Float)
--        print ((myAdd (4.0::Float) (1::Int)) ::Float)

{--
main :: IO ()
main = putStrLn $ show $ myAdd (1::Int) (3.0::Float)
--}

class Coerce a b c where
--class Coerce a b c | a b -> c where
        add :: a -> b -> c

instance Coerce Int Int Int where
        add = (+)

instance Coerce Int Float Float where
        add _ b = b

instance Coerce Float Int Float where
        add a _ = a

instance Coerce Float Float Float where
        add = (+)

--instance Coerce Int Int Float where
--        add _ _ = 0

--doubleAdd :: Int -> Float -> Float
--doubleAdd :: Coerce a b c => a -> b -> c
--doubleAdd a b = add (add a b) (add a b)

myAdd = add

--myAdd' :: Int -> Float -> Float
--myAdd' = myAdd

class Addition a where
        addition :: a -> a -> a

instance Addition Int where
        addition = (+)

instance Addition Float where
        addition = (+)

myAddition = addition

result = myAddition (1::Int) (2::Int)

