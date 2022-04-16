# Functor

```Functors``` are basically things that can be mapped over.

```haskell
class Functor f where
        fmap :: (a -> b) -> f a -> f b
```

Imagine you have a list, ```Maybe```, ```Either``` or some complex tree structure or whatever thing that has ***a context with values of some type inside***, the ```Functor``` class is Haskell's way of defining a standard function that let's you ***work with those values without altering the context*** or structure that holds them.

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

To preserve the structure, ```fmap``` need to adhere to the following:
- Identity
- - ```fmap id == id```
- Composition
- - ```fmap (f . g) == fmap f . fmap g```

The ```<$``` operator is also part of the ```Functor``` class above.
```haskell
(<$) :: a -> f b -> f a
```
It defaults too ```fmap . const``` and just lets you write a more efficient way to replace all the values with a fixed value.

Or its flipped version:
```haskell
($>) :: Functor f => f a -> b -> f b
```

Operator notation:

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

```fmap f a``` is the same as saying ```f `fmap` a``` or ```f <$> a```

You can think of fmap as either a function that takes a function and a functor and then maps that function over the functor, or you can think of it as a function that takes a function and lifts that function so that it operates on functors. Both views are correct and in Haskell, equivalent.

```haskell
ghci> fmap (replicate 3) [1,2,3,4]  
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
ghci> fmap (replicate 3) (Just 4)  
Just [4,4,4]  
ghci> fmap (replicate 3) (Right "blah")  
Right ["blah","blah","blah"]  
ghci> fmap (replicate 3) Nothing  
Nothing  
ghci> fmap (replicate 3) (Left "foo")  
Left "foo"  
```



It says: give me a function that takes an a and returns a b and a box with an a (or several of them) inside it and I'll give you a box with a b (or several of them) inside it. It kind of applies the function to the element inside the box.

A more correct term for what a functor is would be computational context.

If we want to make a type constructor an instance of Functor, it has to have a kind of ```* -> *```, which means that it has to take exactly one concrete type as a type parameter.

```
instance Functor (Either a) where
        fmap f (Right b) = Right (f b)
        fmap _ e = e
```

```fmap``` here has type ```fmap :: (b -> c) -> Either a b -> Either a c```


```haskell
($>)  :: Functor f => f a -> b -> f b 
(<&>) :: Functor f => f a -> (a -> b) -> f b 
void  :: Functor f => f a -> f () 
```

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
