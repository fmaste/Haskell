# Type Families

> Type families essentially provide type-indexed data types and named functions
> on types, which are useful for generic programming and highly parameterized
> library interfaces as well as interfaces with enhanced static information,
> much like dependent types. They might also be regarded as an alternative to
> functional dependencies, but provide a more functional style of type-level
> programming than the relational style of functional dependencies.
>
> [Glasgow Haskell Compiler - 6.4.9. Type families](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_families.html)

## Motivation

After
[A Comparative Study of Language Support for Generic Programming (2003)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.61.9949)
that compares support for different generic programming abstractions in
different programming languages in which Haskell is rated favorably except for
access to the so-called associated types, a group of researchers decided to
tackle this problem in [Associated types with class](https://www.microsoft.com/en-us/research/publication/associated-types-with-class/)

## Associated Data Families

> Haskell’s type classes allow ad-hoc overloading, or type-indexing, of
> functions. A natural generalization is to allow type-indexing of data types as
> well.
>
> [Associated types with class](https://www.microsoft.com/en-us/research/publication/associated-types-with-class/)

Type families come in different flavors but ***associated data families*** are a
rather natural generalization of Haskell’s existing type classes so we are
explaining them first.

Imagine you want to build a generic map library where its representation
depends on the key type used. An specialized map of integer values can be made
more efficient in terms of space and time than a type-invariant parametric
representation.

Dummy example using only ```Int``` as keys:
```haskell
data SuperFastIntMap v = ConsSuperFastIntMap [v]

emptyInt :: SuperFastIntMap v
emptyInt = ConsSuperFastIntMap []

lookupInt :: Int -> SuperFastIntMap v -> Maybe v
lookupInt _ (ConsSuperFastIntMap []) = Nothing
lookupInt _ (ConsSuperFastIntMap (a:_)) = Just a
```

Second dummy example but now using ```Char``` as keys:
```haskell
data SuperEfficientCharMap v = ConsSuperEfficientCharMap [v]

emptyChar :: SuperEfficientCharMap v
emptyChar = ConsSuperEfficientCharMap []

lookupChar :: Char -> SuperEfficientCharMap v -> Maybe v
lookupChar _ (ConsSuperEfficientCharMap []) = Nothing
lookupChar _ (ConsSuperEfficientCharMap (a:_)) = Just a

```

Now like type classes allow ad-hoc overloading or type-indexed functions, that
is to obtain data type specific functionality or functions that can be
instantiated on many data types like ```show```, ```read``` and ```==```, it
will be nice to also have type-indexed data type: A data type that is
constructed from an argument data type in a generic way.

TYPE_INDEXED PERMITS AN AD-HOC OVERLOADING OF TYPES LIKE TYPE CLASSES AND VALUES
Type-indexed data types permit an ad-hoc overloading of types

Imagine you want to build a generic map library where its representation
depends on its element type.

How do you create an abstract interface that allows you write code independent
of the array implementation used?

```haskell
class MapKey k where
        data family Map k v
        empty :: Map k v
        lookup :: k -> Map k v -> Maybe v
```

The family keyword is optional but we wanted to be verbose.

We propose that a type class may define, in addition to a set of methods, a set of associated data types

Data types whose concrete representation depends on one or more type parameters
are called type analysing[15] or type indexed[18]

```haskell
instance MapKey Int where
        data Map Int v = IntMap (SuperFastIntMap v)
        empty = IntMap emptyInt
        lookup k (IntMap intMap) = lookupInt k intMap
```

```haskell
instance MapKey Char where
        data Map Char v = CharMap (SuperEfficientCharMap v)
        empty = CharMap $ ConsSuperEfficientCharMap []
        lookup _ (CharMap (ConsSuperEfficientCharMap [])) = Nothing
        lookup _ (CharMap (ConsSuperEfficientCharMap (a:as))) = Just a

```

## Associated Type Families

# Further Reading

- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_families.html
- https://serokell.io/blog/type-families-haskell
