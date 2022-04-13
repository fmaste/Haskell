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
