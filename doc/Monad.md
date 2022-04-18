# Monads

Remember ```Functor``` and its primary operator ```<$>``` from [here](doc/Applicative.md)?
```haskell
class Functor f where
        fmap :: (a -> b) -> f a -> f b 
        (<$) :: a -> f b -> f a 
```

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b 
```

And ```Applicative``` and its primary function/operator ```<*>``` also from [here](doc/Applicative.md)?

```haskell
class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
```

The Monad class defines the basic operations over a monad, a concept from a branch of mathematics known as [category theory](https://en.wikipedia.org/wiki/Monad_(category_theory)). From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions.

Without further introduction this is a monad in Haskell, just a beefed up applicative functor:
```haskell
class Applicative m => Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        (>>) :: m a -> m b -> m b
        return :: a -> m a 
```

## Historic note

For a long type ```Applicative``` was not a superclass of ```Monad``` but thankfully this was fixed with the [AMP proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal). If you see code not making use of this much needed fix, please understand the past situation!

# Wait, I'm slow


Functors type classes describe additional context using some type.

With Functor this extra structure is often thought of as a "container", while with Monad it tends to be thought of as "side effects".

The distinctive feature of Monad compared to other Functors is that it can embed control flow into the extra structure. The reason it can do this is that, unlike fmap which applies a single flat function over the entire structure, (>>=) inspects individ




# Folds and traversals

## TODO: Link to forall notation

```haskell
class Monad m => MonadFail m where
        fail :: String -> m a
```

```
class Foldable t where
        foldMap :: Monoid m => (a -> m) -> t a -> m 
        foldr :: (a -> b -> b) -> b -> t a -> b 
        foldl :: (b -> a -> b) -> b -> t a -> b 
        foldr1 :: (a -> a -> a) -> t a -> a 
        foldl1 :: (a -> a -> a) -> t a -> a 
        elem :: Eq a => a -> t a -> Bool 
        maximum :: forall a. Ord a => t a -> a 
        minimum :: forall a. Ord a => t a -> a 
        sum :: Num a => t a -> a 
        product :: Num a => t a -> a 
```

```
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
```

```
null :: Foldable t => t a -> Bool
length :: Foldable t => t a -> Int
```

## Special folds

```
and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool
concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```

```
class (Functor t, Foldable t) => Traversable t where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        sequenceA :: Applicative f => t (f a) -> f (t a)
        mapM :: Monad m => (a -> m b) -> t a -> m (t b)
        sequence :: Monad m => t (m a) -> m (t a)
```

```
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m () 
sequence_ :: (Foldable t, Monad m) => t (m a) -> m () 
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

# TODO
[Arrows, like Monads, are Monoids](https://homepages.inf.ed.ac.uk/cheunen/publications/2006/arrows/arrows.pdf)

# TODO: Alternative

A Alt turns any Alternative instance into a Monoid.

https://en.wikibooks.org/wiki/Haskell/Category_theory
