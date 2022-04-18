# Functor

A ```Functor``` would be a computational context.
```Functor```s are basically things that can be mapped over.

```haskell
class Functor f where
        fmap :: (a -> b) -> f a -> f b
```

Imagine you have a list, ```Maybe```, ```Either``` or some complex tree structure or whatever thing that has ***a context with values of some type inside***, the ```Functor``` class is Haskell's way of defining a standard function that let's you ***work with those values without altering the context*** or structure that holds them.

To check that ```fmap``` preserves the structure it needs to adhere to the following rule:
- Identity: ```fmap id == id```

## Examples

Working with values that can be "null" or zero to many values at the same time:

```haskell
instance Functor Maybe where
        fmap f (Just a) = Just (f a)
        fmap _ _ = Nothing

instance Functor [] where
        fmap f xs = map f xs
```

```haskell
> fmap (+1) (Just 1)
Just 2
> fmap (+1) Nothing
Nothing
> fmap (+1) [1,2,3,4,5]
[2,3,4,5,6]
> fmap (+1) []
[]
```

## Operator notation:

```<$>``` is an infix synonym for ```fmap```

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

```f <$> a``` is the same as saying ```f `fmap` a``` or ```fmap f a```

```haskell
> (replicate 3) <$> [1,2,3,4,5]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5]]
> replicate 2 . fmap (+1) <$> [Just 1, Just 2, Nothing, Just 4, Just 5]
[[Just 2,Just 2],[Just 3,Just 3],[Nothing,Nothing],[Just 5,Just 5],[Just 6,Just 6]]
```

The ```<$``` operator is also part of the ```Functor``` class above.
```haskell
(<$) :: a -> f b -> f a
```
It defaults too ```fmap . const``` and just lets you write a more efficient way to replace all the values with a fixed value.

There are flipped version of this two operators that are not used much and don't even come by default with the Prelude:
```haskell
(<&>) :: Functor f => f a -> (a -> b) -> f b -- Flipped version of <$>
($>) :: Functor f => f a -> b -> f b -- Flipped version of <$
```

## Kind

If you haven't noticed when you want to make a type constructor an instance of ```Functor```, it has to have a kind of ```* -> *```, which means that it has to take exactly one concrete type as a type parameter.

```haskell
instance Functor (Either a) where
        fmap f (Right b) = Right (f b)
        fmap _ e = e
```

```fmap``` here has type ```fmap :: (b -> c) -> Either a b -> Either a c```

Only by looking at its type you know it never touches ```a``` values.

## Summary

You can think of fmap as either a function that takes a function and a functor and then maps that function over the functor, or you can think of it as a function that takes a function and lifts that function so that it operates on functors. Both views are correct and in Haskell, equivalent.

It says: give me a function that takes an a and returns a b and a box with an a (or several of them) inside it and I'll give you a box with a b (or several of them) inside it. It kind of applies the function to the element inside the box.

## Further reading

- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
- http://learnyouahaskell.com/functors-applicative-functors-and-monoids

# Applicative

It's Haskell's standard function to map over but now with a function already inside a ```Functor```. With ```fmap``` a function outside the ```Functor``` context is applied to every value inside the context, with ```Applicative``` the function is already inside the ```Functor``` context and sort of "sequences" both ```Functor```s. You can "run" functors without leaving the functor context.

A functor with application, providing operations to:
- embed pure expressions (```pure```).
- sequence computations and combine their results (```<*>```).

```haskell
class (Functor f) => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
```

```Applicative``` is a subclass of ```Functor```, applicative functors are beefed up functors.

More specifically, an ```Applicative``` is an intermediate structure between a ```Functor``` and a ```Monad``` (technically, a strong lax monoidal functor).



Simple instance
```haskell
instance Applicative Maybe where
        pure = Just -- Short for "pure a = Just a"
        Nothing <*> _ = Nothing
        (Just f) <*> b = fmap f b -- Just take out the f and map over.
```

```haskell
liftA2 :: (a -> b -> c) -> f a -> f b -> f c 
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a 
```

## Example

Imagine trying to build a valid ```Person``` data type value from some ```name``` and ```age``` you are parsing from a JSON text, this two can be null or an error or something is wrong.

```haskell
data Person = Person {name :: String, age :: Int}
        deriving Show

parseName :: Maybe String
parseName = Just "Federico"

parseAge :: Maybe Int
parseAge = Just 18
```

All of this are the same
```haskell
> pure Person <*> parseName <*> parseAge
Just (Person {name = "Federico", age = 18})
> fmap Person parseName <*> parseAge
Just (Person {name = "Federico", age = 18})
> Person <$> parseName <*> parseAge
Just (Person {name = "Federico", age = 18})
```

What is this?

First both ```<$>``` and ```<*>``` are left-associative. So ```pure Person <*> parseName <*> parseAge``` is the same as ```((pure Person) <*> parseName) <*> parseAge```.

Injecting a pure non-functor function inside a functor is the same as mapping over a pure function. It's the same as doing ```(fmap Person parseName) <*> parseAge```. Or with its shorthand operator ```<$>``` is the same as ```(Person <$> parseName) <*> parseAge```

How does this work with ```Person```? Easy, see the type of ```Person``` when not all its parameter are applied. It's just like a regular Haskell function.

```haskell
> :t Person
Person :: String -> Int -> Person
> :t Person "Federico"
Person "Federico" :: Int -> Person
> :t Person "Federico" 18
Person "Federico" 18 :: Person
```

What happens when one is ```Nothing```?

```haskell
> Person <$> (Just "Fede") <*> (Just 18)
Just (Person {name = "Fede", age = 18})
> Person <$> Nothing <*> (Just 18)
Nothing
```

A not valid ```Person``` type was found. So ```Nothing``` happened!

# Further reading

- https://www.fpcomplete.com/haskell/tutorial/applicative-syntax/
- https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Applicative.html

# TODO

## TODO
- https://gitlab.haskell.org/ghc/ghc/-/wikis/recursive-do
- https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-do
