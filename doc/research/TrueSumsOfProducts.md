# True Sums of Products

https://www.andres-loeh.de/TrueSumsOfProducts/
https://dl.acm.org/doi/pdf/10.1145/2633628.2633634

```haskell
main :: IO ()
main = do
        print "True Sums of Products!"
```

## Recap of Type-Level Programming

### Kind Signatures With Kind Polymorphism

What we will be doing here is type-level programming. Programming at the type
level is possible as you do at the value level. If you are not familiar with the
concept imagine type constructors as functions at the type level, functions that
return a type. ```Maybe``` is a function of kind ```* -> *```, and
```Maybe Int``` is a type.

Let's define ```I``` as the type equivalent of ```id :: a -> a``` and ```K```
as the type equivalent of ```const :: a -> b -> a```. As in the paper:

```haskell
newtype I (a::*) = I {unI :: a}
```

Result?

```haskell
> ghc -XHaskell2010 src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:9:16: error:
    parse error on input ‘)’
  |
9 | newtype I (a::*) = I {unI :: a}
```

GHC doesn't understand the ```*``` in ```(a::*)```. We need to use kinds or kind
variables like we use type and type variable when defining types. The usual
style with kinds is to use ```k``` for variables:

```haskell
newtype I (a::k) = I {unI :: a}
```

Let's see now:

```haskell
$ ghc -XHaskell2010 src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:11:15: error:
    Unexpected kind variable ‘k’
    Perhaps you intended to use PolyKinds
    In the data type declaration for ‘I’
   |
11 | newtype I (a::k) = I {unI :: a}
   |               ^

src/research/TrueSumsOfProducts.hs:11:15: error:
    Illegal kind signature: ‘k’
      Perhaps you intended to use KindSignatures
    In the data type declaration for ‘I’
   |
11 | newtype I (a::k) = I {unI :: a}
   |
```

We need the
[```KindSignatures``` extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/kind_signatures.html#extension-KindSignatures)
, kind-signature expressions are not part of standard Haskell:

```haskell
{-# LANGUAGE KindSignatures #-}

newtype I (a::k) = I {unI :: a}
```

Results?:

```haskell
$ ghc -XHaskell2010 src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:11:15: error:
    Unexpected kind variable ‘k’
    Perhaps you intended to use PolyKinds
    In the data type declaration for ‘I’
   |
11 | newtype I (a::k) = I {unI :: a}
   | 
```

```k``` is a kind variable and ```a``` inside ```{unI :: a}``` needs to be of
some concrete type. We are "storing" a value inside our newtype, it needs a type
on a statically typed language. We need to import one kind from
[Data.Kind](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Kind.html).
The ```Type``` kind is the kind of types, says that it doesn't need any type
parameter to return a type. ```Type``` is the default kind when kind-signature
expressions are omitted.

```haskell
{-# LANGUAGE KindSignatures #-}

import Data.Kind(Type)

newtype I (a::Type) = I {unI :: a}
```

Now it compiles, let's add the ```const``` like function but at the type level:

```haskell
$ ghc -XHaskell2010 src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:13:25: error:
    Unexpected kind variable ‘k’
    Perhaps you intended to use PolyKinds
    In the data type declaration for ‘K’
   |
13 | newtype K (a::Type) (b::k) = K {unK :: a}
   |
```

What we are trying to say here is that ```b``` can be of any kind, like
```Either```, ```Either a``` or ```Either a b```. So we need the
[```PolyKinds```
extensions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html#extension-PolyKinds)
, if not ```b``` needs to be of a kind that returns a type.

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

import Data.Kind(Type)

newtype I (a::Type) = I {unI :: a}

newtype K (a::Type) (b::k) = K {unK :: a}
```

Great success!

### Datatype promotion

The fundamental idea of the paper and the [sop package](https://hackage.haskell.org/package/generics-sop) is create a friendlier representation of GHC Generics.

```haskell
{-# LANGUAGE TypeFamilies #-}

type family Code (a::Type) :: [[Type]]
```

```haskell
$ ghc -XKindSignatures src/research/TrueSumsOfProducts.hs 
[1 of 1] Compiling Main             ( src/research/TrueSumsOfProducts.hs, src/research/TrueSumsOfProducts.o )

src/research/TrueSumsOfProducts.hs:18:31: error:
    Illegal kind: ‘[[Type]]’ Perhaps you intended to use DataKinds
   |
18 | type family Code (a::Type) :: [[Type]]
   |                               ^^^^^^^^

src/research/TrueSumsOfProducts.hs:18:32: error:
    Illegal kind: ‘[Type]’ Perhaps you intended to use DataKinds
   |
18 | type family Code (a::Type) :: [[Type]]
   |
```

We are saying the the family of types ```Code``` receive a type and return a
type of kind list of lists (```[[]]```). But a list is not a kind, is a type
and a type constructor!!!

We need to use
[Datatype promotion](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html)
that allows promotion of data types to the kind level.

Now the kind ```[]``` is the kind of types ```[]```. Mindblowing!

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#promoted-list-and-tuple-types

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html#ghc-flag--Wunticked-promoted-constructors

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

type family Code (a::Type) :: [[Type]]
```

6.4.12. Representation polymorphism
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/representation_polymorphism.html#runtime-rep

#### Example

Using the example of the paper, let's define an algebraic data type ```Expr```
and its instance of ```Code```:

```haskell
data Expr = Num Int | Add {left :: Expr, right :: Expr}

type instance Code Expr = '[ '[Int], '[Expr, Expr] ]
```

```[Int]``` has kind ```Type``` and ```'[ Int ]``` has kind ```[Type]```.

Are we are doing untyped functional programming at the type level ?

# Further reading

- https://thinkingwithtypes.com/
- [Fun with type functions](https://www.microsoft.com/en-us/research/publication/fun-type-functions/).
