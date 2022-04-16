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
> fmap (replicate 3) [1,2,3,4,5]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5]]
```

## Operator notation:

```fmap f a``` is the same as saying ```f `fmap` a``` or ```f <$> a```

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

Or its flipped version:
```haskell
(<&>) :: Functor f => f a -> (a -> b) -> f b
```

The ```<$``` operator is also part of the ```Functor``` class above.
```haskell
(<$) :: a -> f b -> f a
```
It defaults too ```fmap . const``` and just lets you write a more efficient way to replace all the values with a fixed value.

Or its flipped version:
```haskell
($>) :: Functor f => f a -> b -> f b
```

### Examples

```haskell
> (+1) <$> (Just 1)
Just 2
> (+1) <$> [1,2,3,4,5]
[2,3,4,5,6]
> 1 <$ [1,2,3,4,5]
[1,1,1,1,1]
> [1,2,3,4,5] $> 1
[1,1,1,1,1]
> [1,2,3,4,5] <&> (replicate 3)
[[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5]]
```
## Kind

If you want to make a type constructor an instance of Functor, it has to have a kind of ```* -> *```, which means that it has to take exactly one concrete type as a type parameter.

```
instance Functor (Either a) where
        fmap f (Right b) = Right (f b)
        fmap _ e = e
```

```fmap``` here has type ```fmap :: (b -> c) -> Either a b -> Either a c```

Only by looking at its type you know it never touches ```a``` values.

You can think of fmap as either a function that takes a function and a functor and then maps that function over the functor, or you can think of it as a function that takes a function and lifts that function so that it operates on functors. Both views are correct and in Haskell, equivalent.

It says: give me a function that takes an a and returns a b and a box with an a (or several of them) inside it and I'll give you a box with a b (or several of them) inside it. It kind of applies the function to the element inside the box.

## Further reading

- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
- http://learnyouahaskell.com/functors-applicative-functors-and-monoids

# Applicative

we'll take a closer look at functors, along with slightly stronger and more useful versions of functors called applicative functors.

```haskell
class (Functor f) => Applicative f where  
        pure :: a -> f a  
        (<*>) :: f (a -> b) -> f a -> f b 
```

```haskell
liftA2 :: (a -> b -> c) -> f a -> f b -> f c 
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a 
```

applicative functors, which are beefed up functors
