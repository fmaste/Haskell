# Folds and traversals

## TODO: Link to forall notation

```haskell
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

```haskell
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
```

```haskell
null :: Foldable t => t a -> Bool
length :: Foldable t => t a -> Int
```

## Special folds

```haskell
and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool
concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```

```haskell
class (Functor t, Foldable t) => Traversable t where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        sequenceA :: Applicative f => t (f a) -> f (t a)
        mapM :: Monad m => (a -> m b) -> t a -> m (t b)
        sequence :: Monad m => t (m a) -> m (t a)
```

```haskell
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m () 
sequence_ :: (Foldable t, Monad m) => t (m a) -> m () 
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```
