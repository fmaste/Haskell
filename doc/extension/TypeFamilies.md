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

## Associated Data Families

Type Families come in different flavors but let's explain the first and most
simple one first.

