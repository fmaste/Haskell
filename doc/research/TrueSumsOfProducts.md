# True Sums of Products

Edsko de Vries, Andres Löh

WGP '14 Proceedings of the 10th ACM SIGPLAN workshop on Generic programming, 2014

[Homepage](https://www.andres-loeh.de/TrueSumsOfProducts/)
, [paper](https://dl.acm.org/doi/pdf/10.1145/2633628.2633634)
, [Hackage](https://hackage.haskell.org/package/generics-sop)
, [GitHub](https://github.com/well-typed/generics-sop)

```haskell
main :: IO ()
main = do
        print "True Sums of Products!"
```

SOP list-like structure of datatypes is expressible accurate and allows for the
definition of powerful high-level traversal combinators, which in turn encourage
the definition of generic functions in a compositional and concise style.

## Recap of Type-Level Programming

### Kind Signatures With Kind Polymorphism

What we will be doing here is type-level programming. Programming at the type
level is possible as you do at the term/value level. If you are not familiar
with the concept imagine type constructors as functions at the type level,
functions that return a type. ```Maybe``` is a function of kind ```* -> *```,
and ```Maybe Int``` is a type of kind ```*```.

Let's define ```I``` as the type equivalent of ```id :: a -> a``` and ```K```
as the type equivalent of ```const :: a -> b -> a```. As in the paper:

#### ```I``` - Kind Signatures

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

GHC parser doesn't know if ```::*``` means that ```::*``` altogether is a type
operator or we meant ```:: *``` with a space in between. If we add at least one
space character it compiles correctly.

> Treat the unqualified uses of the ```*``` type operator as nullary and desugar
> to ```Data.Kind.Type```.
>
> The kind ```Type``` (imported from ```Data.Kind```) classifies ordinary types.
> With ```StarIsType``` (currently enabled by default), ```*``` is desugared to
> ```Type```, but using this legacy syntax is not recommended due to conflicts
> with ```TypeOperators```. This also applies to ```★```, the Unicode variant of
> ```*```.
>
> (6.4.11.16. The kind ```Type``` - StarIsType)[https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html#extension-StarIsType]

We can also use kinds or kind variables like we use type and type variable when
defining types. The usual style with kinds is to use ```k``` for variables:

```haskell
{-# LANGUAGE KindSignatures #-}

import Data.Kind(Type)

newtype I (a::Type) = I {unI :: a}
```

Now it compiles, let's add a ```const``` like function but at the type level:

#### ```Const``` - Kind Polymorphism

# TODO: To clean!

```haskell
newtype I (a::k) = I {unI :: a}
```

Results now?:

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

Let's see if you are an intellectual reader:

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
on a statically typed language. We need to use ```*``` or import ```Type``` kind
from
[Data.Kind](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Kind.html).
The ```Type``` kind is the kind of types, like ```*```, says that it doesn't
need any type parameter to return a type. ```*``` or ```Type``` are the default
kind when kind-signature expressions are omitted.

```haskell
newtype K (a::Type) (b::k) = K {unK :: a}
```

Enough. compile for once:

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
, if not ```b``` needs to be of kind ```*``` or ```Type``` that return a type.

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
that automatically promotes every datatype to be a kind and its (value) constructors to be type constructors.

Now the kind ```[]``` is the kind of types ```[]```. Mindblowing!

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#promoted-list-and-tuple-types

As types and constructors can have the same name, to refer to a promoted
constructors prefix it with a single quote mark, like ```'C```.

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

```'[Int, Char, Bool]``` is the same as ```Int ': Char ': Bool : '[]``` and has
kind ```'[Type]```.

Is this an heterogeneous list? No, it's a type-level list of ```Type```. See:

```haskell
ghci> :k '[Int, Char, Bool]
'[Int, Char, Bool] :: [*]
ghci> :k '[Int, Char, Bool, Maybe]

<interactive>:1:1: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
    • In the type ‘'[Int, Char, Bool, Maybe]’
```

Are we are doing untyped functional programming at the type level ?

## Sums and Products

The n-ary sums and products used to translate Haskell values into the SOP
universe are defined as followed.

# Further reading

- https://www.andres-loeh.de/TrueSumsOfProducts/
- https://dl.acm.org/doi/pdf/10.1145/2633628.2633634
- https://thinkingwithtypes.com/
- [Fun with type functions](https://www.microsoft.com/en-us/research/publication/fun-type-functions/).
