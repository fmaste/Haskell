module Main where

main :: IO ()
main = do
        print "Hello!"
--        print (add   (1::Int)   (3::Int))
--        print (add   (1::Float) (3::Float))
        print (myAdd (1::Int)   (3::Int))
--        print (myAdd (1::Float) (3::Float))
{--
        let test addFunction =
                if
                      addFunction (0::Int)   (1::Int) > 0
                   && addFunction (0::Float) (1::Float) > 0
                        then "It works!"
                        else "Something is wrong!"
        print "Testing built-in (+)"
        print $ test (+)
        print "Testing our Addition class"
        print $ test myAdd
--}

class Addition a where
        add :: a -> a -> a

instance Addition Int where
        add = (+)

instance Addition Float where
        add = (+)

--myAdd :: Addition a => a -> a -> a
--myAdd a b = add a b
myAdd = add

--myDoubleAdd a b = add (add a b) (add a b)

--letBound :: (Int, Char)
--letBound = (\f -> (f 1, f 'a')) id

letBound' :: (Int, Char)
letBound' = let f = id in (f 1, f 'a')

letBound'' :: (Int, Char)
letBound'' = (f 1, f 'a')
        where f = id

--mySum = foldl (+) 0

