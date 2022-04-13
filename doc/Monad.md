# Monads and functors

```
class Functor f where
        fmap :: (a -> b) -> f a -> f b 
        (<$) :: a -> f b -> f a 
```

```
(<$>) :: Functor f => (a -> b) -> f a -> f b 
```

## TODO
- https://gitlab.haskell.org/ghc/ghc/-/wikis/recursive-do
- https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-do

```
class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        (*>) :: f a -> f b -> f b
        (<*) :: f a -> f b -> f a
```

For a long type ```Applicative``` was not a superclass of ```Monad``` but thankfully this was fixed with the [AMP proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal). If you see code not making use of this much needed fix, please understand the past situation!

```
class Applicative m => Monad m where
        (>>=) :: forall a b. m a -> (a -> m b) -> m b
        (>>) :: forall a b. m a -> m b -> m b
        return :: a -> m a 
```

```
class Monad m => MonadFail m where
        fail :: String -> m a 
```

# Folds and traversals

## TODO: Link to forall notation

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