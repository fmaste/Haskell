module Main where

main :: IO ()
main = do
        print "Hello!"
        print (add (1::Int) (3::Int))
        print (myAdd (1::Float) (3::Float))

--myAdd' :: Int -> Float -> Float
--myAdd' = myAdd

class Addition a where
        add :: a -> a -> a

instance Addition Int where
        add = (+)

instance Addition Float where
        add = (+)

myAdd :: Addition a => a -> a -> a
myAdd = add

myDoubleAdd a b = add (add a b) (add a b)

--result = myAddition (1::Int) (2::Int)

