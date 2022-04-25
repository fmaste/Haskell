module Main where

main :: IO ()
main = do
        print "Hello!"

mySum xs = foldl (+) 0 xs

