```haskell
data List a = Nil | Cons a (List a)
        deriving Show
```

```haskell
instance Semigroup (List a) where
        Nil <> xs = xs
        (Cons a xs) <> ys = (Cons a $ xs <> ys)
```

```haskell
instance Monoid (List a) where
        mempty = Nil
```

```haskell
instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons a xs) = Cons (f a) (fmap f xs)
```

```haskell
instance Applicative List where
        pure a = Cons a Nil
        Nil <*> _ = Nil
        _ <*> Nil = Nil
        (Cons f xs) <*> ys = (fmap f ys) <> (xs <*> ys)
```

```haskell
instance Monad List where
        return a = Cons a Nil
        Nil >>= _ = Nil
        (Cons a xs) >>= f = f a <> (xs >>= f)
```
