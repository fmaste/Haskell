# Multi-parameter type classes

Type class declarations are as defined by the 2010 standard of the form (See type classes):
```haskell
class C a where
        f :: a -> a -> a
````
This extensions allows:
```haskell
class C' a b c where
        f' :: a -> b -> c
````

Also called a parametric type class, a class that has type parameters in
addition to the placeholder variable which is always present in a class
declaration.

## Usage

Multi-parameter type classes are permitted with pragma ```MultiParamTypeClasses```.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

class Collection c a where
        union :: c a -> c a -> c a

```

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Coerce a b c | a b -> c where
        add :: a -> b -> c
```

## Why not part of the standard?

This extension was not part of the Haskell 98 standard neither 2010.

> Single-parameter type classes were already a big step beyond our initial
> conservative design goals, and they solved the problem we initially addressed
> (overloading equality and numeric operations). Going beyond that would be an
> unforced step into the dark, and we were anxious about questions of overlap,
> confluence, and decidability of type inference. While it was easy to define
> coerce as above, it was less clear when type inference would make it usable in
> practice. As a result, Haskell 98 retained the single-parameter restriction.
>
> Section 6.5 from ["A History of Haskell: Being Lazy with Class"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf)

> ... ‘multiple parameters’ allows a more general interpretation of classes as
> relations on types, and has many potentially useful applications.
> Unfortunately, many of these examples do not work well in practice, leading to 
> ambiguities and inaccuracies in inferred types and delaying the detection of
> type errors.
>
> [Type Classes with Functional Dependencies, Mark P. Jones, In Proceedings of the 9th European Symposium on Programming, ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html).

## Type checker and type inference in action

Define this type class and 3 instance declarations:
```haskell
class Coerce a b c where
        add :: a -> b -> c

instance Coerce Int Int Int where
        add = (+)

instance Coerce Int Float Float where
        add _ b = b -- Just for fun.

instance Coerce Float Int Float where
        add a _ = a -- Just for fun.

instance Coerce Float Float Float where
        add = (+)
```

If you try to add function ```myAdd``` without any type information as show
below the compiler must fail.

```haskell
myAdd = add
```

```haskell
> ghci -fprint-potential-instances src/MultiParamTypeClasses.hs
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/MultiParamTypeClasses.hs, interpreted )

src/MultiParamTypeClasses.hs:31:9: error:
    • Ambiguous type variables ‘a0’, ‘b0’,
                               ‘c0’ arising from a use of ‘add’
      prevents the constraint ‘(Coerce a0 b0 c0)’ from being solved.
      Relevant bindings include
        myAdd :: a0 -> b0 -> c0
          (bound at src/MultiParamTypeClasses.hs:31:1)
      Probable fix: use a type annotation to specify what ‘a0’, ‘b0’,
                                                          ‘c0’ should be.
      These potential instances exist:
        instance Coerce Float Float Float
          -- Defined at src/MultiParamTypeClasses.hs:27:10
        instance Coerce Float Int Float
          -- Defined at src/MultiParamTypeClasses.hs:24:10
        instance Coerce Int Float Float
          -- Defined at src/MultiParamTypeClasses.hs:21:10
        instance Coerce Int Int Int
          -- Defined at src/MultiParamTypeClasses.hs:18:10
    • In the expression: add
      In an equation for ‘myAdd’: myAdd = add
   |
31 | myAdd = add
   |         ^^^
Failed, no modules loaded.
```

What we must learn from this example error is that Haskell is a statically typed
language, every expression in Haskell has a type which must be determined at
compile time and not when running the already generated executable code.

As there's no caller of this function the compiler has no way to know which
implementation is intended to be used, it can't choose one implementation and
hence infer the type of ```myAdd```. Imagine what could happen if it chooses an
unintended implementation, ```1 + 2``` could become ```4```, who knows!

In contrast with dynamically typed languages all the types composed together by
function application have to match up. If they don't, the program will be
rejected by the compiler.

We could say that this was a problem of the type inference system and as with
most type error in Haskell, with proper type annotations it should work.
```haskell
main :: IO ()
main = do
        print ((myAdd (1::Int) (3.0::Float)) :: Float)
```
```haskell
ghci> :t myAdd
myAdd :: Int -> Float -> Float
ghci> main
3.0
```

Now before trying to add any type information to the ```myAdd``` function, try
calling it twice with different types like this:
```haskell
main :: IO ()
main = do
        print ((myAdd (1::Int) (3.0::Float)) :: Float)
        print ((myAdd (4.0::Float) (1::Int)) :: Float)

```
```haskell
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/MultiParamTypeClasses.hs, interpreted )

src/MultiParamTypeClasses.hs:9:24: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Float’
    • In the first argument of ‘myAdd’, namely ‘(4.0 :: Float)’
      In the first argument of ‘print’, namely
        ‘((myAdd (4.0 :: Float) (1 :: Int)) :: Float)’
      In a stmt of a 'do' block:
        print ((myAdd (4.0 :: Float) (1 :: Int)) :: Float)
  |
9 |         print ((myAdd (4.0::Float) (1::Int)) :: Float)
  |                        ^^^^^^^^^^

src/MultiParamTypeClasses.hs:9:37: error:
    • Couldn't match expected type ‘Float’ with actual type ‘Int’
    • In the second argument of ‘myAdd’, namely ‘(1 :: Int)’
      In the first argument of ‘print’, namely
        ‘((myAdd (4.0 :: Float) (1 :: Int)) :: Float)’
      In a stmt of a 'do' block:
        print ((myAdd (4.0 :: Float) (1 :: Int)) :: Float)
  |
9 |         print ((myAdd (4.0::Float) (1::Int)) :: Float)
  |                                     ^^^^^^
Failed, no modules loaded.
```
The type inference system is good but not that good while trying to be
unambiguous. With the first usage parsed it inferred that the type was
```myAdd :: Int -> Float -> Float``` but later you are calling it with type
```myAdd :: Float -> Int -> Float```.


Now we can add the most abstract type as possible to ```myAdd``` or call the
class member function ```add``` directly and everything will work as expected.
```haskell
main :: IO ()
main = do
        print ((add (1::Int) (3.0::Float)) :: Float)
        print ((add (4.0::Float) (1::Int)) :: Float)

myAdd :: Coerce a b c => a -> b -> c
myAdd = add
```
```haskell
ghci> main
3.0
4.0
```

The typing fun doesn't end here. Remove the final type annotation of the calls
to ```add``` expecting the compiler to infer ```Float```. Because in the end the
available instances are:
- ```Coerce Int Int Int```
- ```Coerce Float Int Float```
- ```Coerce Int Float Float```
- ```Coerce Float Float Float```
and the result is obvious. Is it obvious?
```haskell
main :: IO ()
main = do
        print (add (1::Int) (3.0::Float))
        print (add (4.0::Float) (1::Int))
```
```haskell
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/MultiParamTypeClasses.hs, interpreted )

src/MultiParamTypeClasses.hs:8:9: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
      prevents the constraint ‘(Show a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Show Ordering -- Defined in ‘GHC.Show’
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        instance Show Integer -- Defined in ‘GHC.Show’
        ...plus 23 others
        ...plus 13 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of a 'do' block: print (add (1 :: Int) (3.0 :: Float))
      In the expression:
        do print (add (1 :: Int) (3.0 :: Float))
           print (add (4.0 :: Float) (1 :: Int))
      In an equation for ‘main’:
          main
            = do print (add (1 :: Int) (3.0 :: Float))
                 print (add (4.0 :: Float) (1 :: Int))
  |
8 |         print (add (1::Int) (3.0::Float))
  |         ^^^^^

src/MultiParamTypeClasses.hs:8:16: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘add’
      prevents the constraint ‘(Coerce Int Float a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instance exist:
        instance Coerce Int Float Float
          -- Defined at src/MultiParamTypeClasses.hs:23:10
    • In the first argument of ‘print’, namely
        ‘(add (1 :: Int) (3.0 :: Float))’
      In a stmt of a 'do' block: print (add (1 :: Int) (3.0 :: Float))
      In the expression:
        do print (add (1 :: Int) (3.0 :: Float))
           print (add (4.0 :: Float) (1 :: Int))
  |
8 |         print (add (1::Int) (3.0::Float))
  |                ^^^

src/MultiParamTypeClasses.hs:9:9: error:
    • Ambiguous type variable ‘a1’ arising from a use of ‘print’
      prevents the constraint ‘(Show a1)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a1’ should be.
      These potential instances exist:
        instance Show Ordering -- Defined in ‘GHC.Show’
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        instance Show Integer -- Defined in ‘GHC.Show’
        ...plus 23 others
        ...plus 13 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of a 'do' block: print (add (4.0 :: Float) (1 :: Int))
      In the expression:
        do print (add (1 :: Int) (3.0 :: Float))
           print (add (4.0 :: Float) (1 :: Int))
      In an equation for ‘main’:
          main
            = do print (add (1 :: Int) (3.0 :: Float))
                 print (add (4.0 :: Float) (1 :: Int))
  |
9 |         print (add (4.0::Float) (1::Int))
  |         ^^^^^

src/MultiParamTypeClasses.hs:9:16: error:
    • Ambiguous type variable ‘a1’ arising from a use of ‘add’
      prevents the constraint ‘(Coerce Float Int a1)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a1’ should be.
      These potential instance exist:
        instance Coerce Float Int Float
          -- Defined at src/MultiParamTypeClasses.hs:26:10
    • In the first argument of ‘print’, namely
        ‘(add (4.0 :: Float) (1 :: Int))’
      In a stmt of a 'do' block: print (add (4.0 :: Float) (1 :: Int))
      In the expression:
        do print (add (1 :: Int) (3.0 :: Float))
           print (add (4.0 :: Float) (1 :: Int))
  |
9 |         print (add (4.0::Float) (1::Int))
  |                ^^^
Failed, no modules loaded.
```

# Functional dependencies to the rEsCuE



> In a predicate such as ```Eq a```, we refer to ```Eq``` as the class name, and
> to ```a``` as the class parameter. Were it not for the use of a restricted
> character set, constraints like this might instead have been written in the
> form ```a ∈ Eq```, reflecting an intuition that ***```Eq``` represents a set
> of types of which ```a``` is expected to be a member***. The Haskell syntax,
> however, which looks more like a curried function application, suggests that
> it might be possible to allow classes to have more than one parameter. For
> example, what might a predicate of the form ```R a b``` mean, where two
> parameters ```a``` and ```b``` have been provided? The obvious answer is to
> interpret ```R``` as a two-place relation between types, and to read
> ```R a b``` as the assertion that ```a``` and ```b``` are related by ```R```.
> This is a natural generalization of the one parameter case because sets are
> just one-place relations. More generally, we can interpret an n parameter
> class by an n-place relation on types.
>
> [Type Classes with Functional Dependencies, Mark P. Jones, In Proceedings of the 9th European Symposium on Programming, ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html).

Naive use of MPTCs may result in ambiguity, so functional dependencies were
developed as a method of resolving that ambiguity, declaring that some subset of
the parameters is sufficient to determine the values of the others.

The key idea is to allow the definitions of type classes to be annotated
with functional dependencies - an idea that originates in the theory of
relational databases

```haskell
class Add' a b c | a b -> c where
        add' :: a -> b -> c

instance Add' Int Int Int where
        add' = (+)
```

```haskell
> add' (1::Int) (1::Int)
2
```

## Final personal note

IMO Multi-parameter type classes are [not a good idea since type families arrived](https://wiki.haskell.org/Functional_dependencies_vs._type_families). One of the intended uses of this extension was to generalize list abstractions and concepts to monads and those are possible, at least now, without this extension (See Monoid, Semigroup, etc).

It allowed to build many good libraries like [```mtl``` monad transformer library](https://hackage.haskell.org/package/mtl) for a long time. But now it's just classes that abstract another library with the same functionality built as a portable package (no multi-parameter) in [```transformers```](https://hackage.haskell.org/package/transformers). Many package using ```mtl``` can be ported to ```transformers``` with only slight modifications.

A [type families based version](https://hackage.haskell.org/package/monads-tf) appeared later.

Don't get me wrong, the idea of a statically typed language is to accept as many good programs as possible and reject as many bad ones as possible.

## Further reading

- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/multi_param_type_classes.html
- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/functional_dependencies.html
- https://wiki.haskell.org/Multi-parameter_type_class
- [Chen, K., Hudak, P., and Odersky, M. (1992). Parametric type classes. In Proceedings of ACM Conference on Lisp and Functional Programming, pages 170–181. ACM](https://dl.acm.org/doi/abs/10.1145/141471.141536).
- General framework for qualified types:
  - [Jones, M. (1992). A theory of qualified types. In European Symposium on Programming (ESOP’92), number 582 in Lecture Notes in Computer Science, Rennes, France. Springer Verlag](https://core.ac.uk/download/pdf/82271317.pdf).
  - [Jones, M. (1991). Type inference for qualified types. PRG-TR-10-91, Programming Research Group, Oxford, Oxford University](https://www.cs.ox.ac.uk/techreports/oucl/index2.html).
- [Type Classes with Functional Dependencies, Mark P. Jones, In Proceedings of the 9th European Symposium on Programming, ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html).
