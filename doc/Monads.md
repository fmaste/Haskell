# Semigroups and Monoids

## [Semigroups](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Semigroup)

The class of types that have an associative binary operation. A very general typeclass.

```
class Semigroup a where
        (<>) :: a -> a -> a
```

Instances should satisfy the following:
- Associativity: ```x <> (y <> z) = (x <> y) <> z```

In essence, the <> function could do anything, as long as it doesn't matter where you put parenthesis. ```8 `div` (4 `div` 2) == 8 `div` 2 == 4``` is not equal to ```(8 `div` 4) `div` 2 == 2 `div` 2 == 1```.

## [Monoids](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Monoid.html#t:Monoid)

In Haskell a Monoid (not to be confused with ```Monad```) is a ```Semigroup``` with the added requirement of a neutral element.

```
class Semigroup a => Monoid a where
        mempty :: a
        mappend :: a -> a -> a
        mconcat :: [a] -> a
```

Instances should satisfy the following:
- Right identity: ```x <> mempty = x```
- Left identity: ```mempty <> x = x```
- Associativity: ```x <> (y <> z) = (x <> y) <> z``` (Semigroup law)
- Concatenation: ```mconcat = foldr (<>) mempty```

[```foldr```](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-List.html#v:foldr) references the [Lists](doc/Lists.md) definition, not the ```foldable``` one.

 It's a class for types which have a ***single most natural operation for combining values***, together with a value which doesn't do anything when you combine it with others (this is called the identity element).

Emphasis on ***single*** because numbers also form a monoid under addition, with 0 the identity element, but they also form a monoid under multiplication, with 1 the identity element. Neither of these instances are really more natural than the other, so we use the newtypes [Sum](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Semigroup.html#v:Sum) and [Product](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Semigroup.html#v:Product) to distinguish between them.

It is closely related to the ```Foldable``` class, and indeed you can think of a Monoid instance declaration for a type ```a``` as precisely what you need in order to fold up a list of values of m. 

See more:
- (http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html)

## Defining

Example:

```
instance Semigroup [a] where
        (<>) = (++)
```

```
instance Monoid [a] where
        mappend = (<>) -- From the Semigroup instance above.
        mempty = []
```

The ```mconcat``` definition is derived from the other two if none is provided.

# Monads and functors

```
class Functor f where
        fmap :: (a -> b) -> f a -> f b 
        (<$) :: a -> f b -> f a 
```

```
(<$>) :: Functor f => (a -> b) -> f a -> f b 
```

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
