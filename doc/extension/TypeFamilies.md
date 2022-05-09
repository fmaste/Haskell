# Type Families

> Type families essentially provide type-indexed data types and named functions
> on types, which are useful for generic programming and highly parameterized
> library interfaces as well as interfaces with enhanced static information,
> much like dependent types. They might also be regarded as an alternative to
> functional dependencies, but provide a more functional style of type-level
> programming than the relational style of functional dependencies.
>
> [Glasgow Haskell Compiler - 6.4.9. Type families](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html)

## Motivation

After
[A Comparative Study of Language Support for Generic Programming (2003)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.61.9949)
that compares support for different generic programming abstractions in
different programming languages in which Haskell is rated favorably except for
access to the so-called associated types, a group of researchers decided to
tackle this problem.

The ```TypeFamilies``` extension was first introduced in
[Associated types with class](https://www.microsoft.com/en-us/research/publication/associated-types-with-class/)

> Haskell’s type classes allow ad-hoc overloading, or type-indexing, of
> functions. A natural generalization is to allow type-indexing of data types as
> well.
>
> [Associated types with class](https://www.microsoft.com/en-us/research/publication/associated-types-with-class/)

Type Families come in different flavors but let's explain the first and most
simple one first.

## Associated Data Families

Imagine you want to build a generic array library for that depends on its
element type.

Now like John Lennon "Imagine there's no countries" and imagine you have
discovered a super fast map implementation for when you are using ```Int``` as
key:
```haskell
data SuperFastIntMap v = ConsSuperFastIntMap [v]

emptyInt :: SuperFastIntMap v
emptyInt = ConsSuperFastIntMap []

lookupInt :: Int -> SuperFastIntMap v -> Maybe v
lookupInt _ (ConsSuperFastIntMap []) = Nothing
lookupInt _ (ConsSuperFastIntMap (a:_)) = Just a
```

And "Imagine no possessions" and a super space efficient map but only for
```Char``` keys:
```haskell
data SuperEfficientCharMap v = ConsSuperEfficientCharMap [v]

emptyChar :: SuperEfficientCharMap v
emptyChar = ConsSuperEfficientCharMap []

lookupChar :: Char -> SuperEfficientCharMap v -> Maybe v
lookupChar _ (ConsSuperEfficientCharMap []) = Nothing
lookupChar _ (ConsSuperEfficientCharMap (a:_)) = Just a

```

```haskell
class MapKey k where
        data Map k v
        empty :: Map k v
        lookup :: k -> Map k v -> Maybe v
```

```haskell
```
