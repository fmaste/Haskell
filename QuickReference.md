# [Prelude](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html) from [base](https://hackage.haskell.org/package/base) version 4.16.1.0 

## Bool

```
data Bool = True | False
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool
not :: Bool -> Bool -> Bool
otherwise :: Bool
```
## Maybe and Either

```
data Maybe a = Nothing | Just a
maybe :: b -> (a -> b) -> Maybe a -> b
```
```
data Either a b = Left a | Right b
either :: (a -> c) -> (b -> c) -> Either a b -> c
```

## Tuples

```
fst :: (a,b) -> a
snd :: (a,b) -> b
curry :: ((a,b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a,b) -> c
```

## Compare

```
data Ordering = LT | EQ | GT

class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

class Eq a => Ord a where
        compare :: a -> a -> Ordering
        (<) :: a -> a -> Bool
        (<=) :: a -> a -> Bool
        (>) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        max :: a -> a -> a
        min :: a -> a -> a
```

## Builtin types

### Natural numbers

```
data Int
data Integer
data Float
data Double
```

### TODO
```
type Rational = Ratio Integer

data Word
```

## Characters

```
data Char
type String = [Char]
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

