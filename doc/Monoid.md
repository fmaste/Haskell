# Semigroups and Monoids

## [Semigroups](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Semigroup)

The class of types that have an associative binary operation. A very general typeclass.

```haskell
class Semigroup a where
        (<>) :: a -> a -> a
```

Instances should satisfy the following:
- Associativity: ```x <> (y <> z) = (x <> y) <> z```

In essence, the <> function could do anything, as long as it doesn't matter where you put parenthesis. ```8 `div` (4 `div` 2) == 8 `div` 2 == 4``` is not equal to ```(8 `div` 4) `div` 2 == 2 `div` 2 == 1```.

## [Monoids](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Monoid.html#t:Monoid)

In Haskell a Monoid (not to be confused with ```Monad```) is a ```Semigroup``` with the added requirement of a neutral element.

```haskell
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

In general, if you're going to define an instance of monoid for your datatype, then the ***most straightforward and useful definition*** should be chosen.

It is closely related to the ```Foldable``` class, and indeed you can think of a Monoid instance declaration for a type ```a``` as precisely what you need in order to fold up a list of values of m. 

## Defining

Example:

```haskell
instance Semigroup [a] where
        (<>) = (++)
```

```haskell
instance Monoid [a] where
        mappend = (<>) -- From the Semigroup instance above.
        mempty = []
```

The ```mconcat``` definition is derived from the other two if none is provided.

## Special ```Monoid``` types

Like ```Sum``` and ```Product``` explained above there are other wrappers

### Boolean wrappers

```haskell
newtype All = All {getAll :: Bool}
>>> getAll (All True <> mempty <> All False)
False
>>> getAll (mconcat (map (\x -> All (even x)) [2,4,6,7,8]))
False
```

```haskell
newtype Any = Any {getAny :: Bool}
>>> getAny (Any True <> mempty <> Any False)
True
>>> getAny (mconcat (map (\x -> Any (even x)) [2,4,6,7,8]))
True
```

### Reverse wrapper

```haskell
newtype Dual a = Dual {getDual :: a}

>>> getDual (mappend (Dual "Hello") (Dual "World"))
"WorldHello"
```

### Function wrapper

```haskell
newtype Endo a = Endo {appEndo :: a -> a}
>>> let computation = Endo ("Hello, " ++) <> Endo (++ "!")
>>> appEndo computation "Haskell"
"Hello, Haskell!"
```

```Endo``` comes from an "Endomorphism" in category theory, means that functions with of type ```a -> a``` can be combined by composition, and the order of the application of composition doesn't matter much.

## Further reading:

- https://wiki.haskell.org/Typeclassopedia#Further_reading_6
- https://www.schoolofhaskell.com/user/mgsloan/monoids-tour
- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
- https://apfelmus.nfshost.com/articles/monoid-fingertree.html
- https://www.staff.city.ac.uk/~ross/papers/FingerTree.html
