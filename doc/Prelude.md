# [Prelude](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html) from [base](https://hackage.haskell.org/package/base) version 4.16.1.0 

## Builtin types

### Bool

```
data Bool = True | False
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool
not :: Bool -> Bool -> Bool
otherwise :: Bool
```

### Natural numbers

A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]
```
data Int
```
The entire infinite range of integers
```
data Integer
```
Single-precision floating point numbers.
```
data Float
```
Double-precision floating point numbers.
```
data Double
```

### TODO

```
type Rational = Ratio Integer # Arbitrary-precision rational numbers

data Word # A Word is an unsigned integral type, with the same size as Int.
```

### Maybe and Either

#### Maybe

How to represent a value that can be null?

```
data Maybe a = Nothing | Just a
maybe :: b -> (a -> b) -> Maybe a -> b
```
More functions in module [Data.Maybe](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Maybe.html)

#### Either

How to represent a value that can be an error (The standard practice is using ```Left```)?

```
data Either a b = Left a | Right b
either :: (a -> c) -> (b -> c) -> Either a b -> c
```

More functions in module [Data.Either](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Either.html)

## Basic type classes

### Compare

As long as a type is an instance of class ```Eq```, you can apply operators ```==``` and ```/=``` to values of that type

```
data Ordering = LT | EQ | GT
```

```
class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
```

### Order

As long as a type is an instance of class ```Ord```, you can apply operators ```>```, ```>=```, ```<``` and ```<=``` to values of that type

```
class Eq a => Ord a where
        compare :: a -> a -> Ordering
        (<) :: a -> a -> Bool
        (<=) :: a -> a -> Bool
        (>) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        max :: a -> a -> a
        min :: a -> a -> a
```

```
class Enum a where
        succ :: a -> a
        pred :: a -> a
        toEnum :: Int -> a
        fromEnum :: a-> Int
        enumFrom :: a-> [a]
        enumFromThen :: a-> a -> [a]
        enumFromTo :: a-> a -> [a]
        enumFromThenTo :: a-> a -> a -> [a]
```

```
class Bounded a where
        minBound :: a
        minBound :: a
```

## Tuples

```
fst :: (a,b) -> a
snd :: (a,b) -> b
curry :: ((a,b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a,b) -> c
```

## Lists

See [Lists](Lists.md) for list functions

## Characters

```
data Char
type String = [Char]
```

See [Strings](Strings.md) for String functions

## Numeric type classes

As long as a type is an instance of class ```Num```, you can apply operators ```+```, ```-``` and ```*```

```
class Num a where
        (+) :: a -> a -> a
        (-) :: a -> a -> a
        (*) :: a -> a -> a
        negate :: a -> a
        abs :: a -> a
        signum :: a -> a
        fromInteger :: Integer -> a
```

```
class (Num a, Ord a) => Real a where
        toRational :: a -> Rational
```

```
class (Real a, Enum a) => Integral a where
        quot :: a -> a -> a
        rem :: a -> a -> a
        div :: a -> a -> a
        mod :: a -> a -> a
        quotRem :: a -> a -> (a, a) 
        divMod :: a -> a -> (a, a) 
        toInteger :: a -> Integer
```

#### Fractional

See [Fractional](NumbersFractional.md)

# Basic Input and output

```
data IO
```

## Output functions

```
putChar :: Char -> IO () 
putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
```

## Input functions

```
getChar :: IO Char
getLine :: IO String
getContents :: IO String
interact :: (String -> String) -> IO ()
```

## Files

```
type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()
readIO :: Read a => String -> IO a
readLn :: Read a => IO a
```

## Exception handling in the I/O monad

```
type IOError = IOException
ioError :: IOError -> IO a
userError :: String -> IOError
```
