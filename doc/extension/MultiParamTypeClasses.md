# Multi-parameter type classes

Also called a parametric type class, a class that has type parameters in
addition to the placeholder variable which is always present in a class
declaration.

Multi-parameter type classes are permitted with extension ```MultiParamTypeClasses```.

## Example

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Collection c a where
        union :: c a -> c a -> c a

class Coerce a b c | a b -> c where
        add :: a -> b -> c
```

## Why?

> Single-parameter type classes were already a big step beyond our initial conservative design goals, and they solved the problem we initially addressed (overloading equality and numeric operations). Going beyond that would be an unforced step into the dark, and we were anxious about questions of overlap, confluence, and decidability of type inference. While it was easy to define coerce as above, it was less clear when type inference would make it usable in practice. As a result, Haskell 98 retained the single-parameter restriction.
>
> Section 6.5 from ["A History of Haskell: Being Lazy with Class"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf)

IMO Multi-parameter type classes are a good idea if used with type families. One of the intended uses of this extension was to generalize list abstractions and concepts to monads and those are possible, at least now, without this extension (See Monoid, Semigroup, etc).

```haskell
instance Semigroup [a] where
        ...

instance Monoid [a] where
        ...

instance Functor [] where
        ...

instance Applicative [] where
        ...

instance Monad []  where
        ...
```

## Further reading

- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/multi_param_type_classes.html
- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/functional_dependencies.html
- [Chen, K., Hudak, P., and Odersky, M. (1992). Parametric type classes. In Proceedings of ACM Conference on Lisp and Functional Programming, pages 170–181. ACM](https://dl.acm.org/doi/abs/10.1145/141471.141536).
- General framework for qualified types:
  - [Jones, M. (1992). A theory of qualified types. In European Symposium on Programming (ESOP’92), number 582 in Lecture Notes in Computer Science, Rennes, France. Springer Verlag](https://core.ac.uk/download/pdf/82271317.pdf).
  - Jones, M. (1991). Type inference for qualified types. PRG-TR-10-91, Programming Research Group, Oxford, Oxford University.
