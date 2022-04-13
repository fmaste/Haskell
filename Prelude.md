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

### Characters

```
data Char
type String = [Char]
```

### Tuples

```
fst :: (a,b) -> a
snd :: (a,b) -> b
curry :: ((a,b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a,b) -> c
```

### Maybe and Either

```
data Maybe a = Nothing | Just a
maybe :: b -> (a -> b) -> Maybe a -> b
```
```
data Either a b = Left a | Right b
either :: (a -> c) -> (b -> c) -> Either a b -> c
```

## Basic type classes

### Compare

```
data Ordering = LT | EQ | GT
```

```
class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
```

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

### Order

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

### Numeric type classes

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

# List operations

```
map :: (a -> b) -> [a] -> [b]
(++) :: [a] -> [a] -> [a]
filter :: (a -> Bool) -> [a] -> [a]
head :: [a] -> a
last :: [a] -> a
tail :: [a] -> [a]
init :: [a] -> [a]
(!!) :: [a] -> Int -> a
reverse :: [a] -> [a]
```

# Building lists

```
scanl :: (b -> a -> b) -> b -> [a] -> [b] 
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]
```

# Infinite lists

```
iterate :: (a -> a) -> a -> [a]
repeat :: a -> [a]
replicate :: Int -> a -> [a]
cycle :: [a] -> [a]
```

# Sublists

```
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
span :: (a -> Bool) -> [a] -> ([a], [a])
break :: (a -> Bool) -> [a] -> ([a], [a])
splitAt :: Int -> [a] -> ([a], [a])
```

# Searching lists

```
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```

# Zipping and unzipping lists

```
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
```
