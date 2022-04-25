# Type Checking and Type Inference in Action

The most difficult part in Haskell for beginners is the type system and the
compiler's error messages. Most of this arise from how type classes work and how
we are used to think about classes and implementations in object oriented
languages or dynamically typed languages.

## By example

Define this type class and 2 instance declarations:

```haskell
class Addition a where
        add :: a -> a -> a

instance Addition Int where
        add = (+)

instance Addition Float where
        add a _ = b -- Just for fun
```

### Unambiguous Overloading

If you try to add function ```myAdd``` without a type-signatures expression as
show below:

```haskell
myAdd = add
```

Such expression is considered ill-typed, a static type error:

```haskell
> ghci -XHaskell2010 src/TypeCheckingAndInference.hs
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/TypeCheckingAndInference.hs, interpreted )

src/TypeCheckingAndInference.hs:20:9: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘add’
      prevents the constraint ‘(Addition a0)’ from being solved.
      Relevant bindings include
        myAdd :: a0 -> a0 -> a0
          (bound at src/TypeCheckingAndInference.hs:20:1)
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Addition Float
          -- Defined at src/TypeCheckingAndInference.hs:17:10
        instance Addition Int
          -- Defined at src/TypeCheckingAndInference.hs:14:10
    • In the expression: add
      In an equation for ‘myAdd’: myAdd = add
   |
20 | myAdd = add
   |         ^^^
Failed, no modules loaded.
```

What we must learn from this example error is that Haskell is a statically typed
language, ***every expression in Haskell has a type which must be determined at
compile time*** and not when already running the generated executable code.

As there's no caller of this function ***the compiler has no way to know which
implementation is intended to be used***, it can't choose one implementation and
hence infer the type of ```myAdd```. Imagine what could happen if it chooses an
unintended implementation, ```1 + 2``` could become ```4```, who knows!

We could say that this was a problem of the type inference system and ***ambiguous
types in Haskell can only be circumvented by input from the user with proper
type-signature expressions***:

```haskell
main :: IO ()
main = do
        print "Hello!"
        print (myAdd (1::Int) (3::Int))
```

```haskell
ghci> :t myAdd
myAdd :: Int -> Int -> Int
ghci> main
"Hello!"
4
```

### No Implicit Overloading

Now before trying to add a type-signature to the ```myAdd``` function, try
calling it twice with different input types like this:

```haskell
main :: IO ()
main = do
        print "Hello again!"
        print (myAdd (1::Int) (3::Int))
        print (myAdd (1::Float) (3::Float))
```

```haskell
> ghci -XHaskell2010 src/TypeCheckingAndInference.hs
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/TypeCheckingAndInference.hs, interpreted )

src/TypeCheckingAndInference.hs:7:23: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Float’
    • In the first argument of ‘myAdd’, namely ‘(1 :: Float)’
      In the first argument of ‘print’, namely
        ‘(myAdd (1 :: Float) (3 :: Float))’
      In a stmt of a 'do' block: print (myAdd (1 :: Float) (3 :: Float))
  |
7 |         print (myAdd (1::Float) (3::Float))
  |                       ^^^^^^^^

src/TypeCheckingAndInference.hs:7:34: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Float’
    • In the second argument of ‘myAdd’, namely ‘(3 :: Float)’
      In the first argument of ‘print’, namely
        ‘(myAdd (1 :: Float) (3 :: Float))’
      In a stmt of a 'do' block: print (myAdd (1 :: Float) (3 :: Float))
  |
7 |         print (myAdd (1::Float) (3::Float))
  |                                  ^^^^^^^^
Failed, no modules loaded.
```

The same way the compiler couldn't pick a specific implementation of class
```Addition``` it can't make function ```myAdd``` use restricted polymorphism /
overloading by its own. ***How can the compiler be unambiguously sure what the
developer wants?***

The type inference system is good but can't be that good while trying to be
unambiguous. With the first usage parsed it inferred that the type of
```myAdd``` was ```myAdd :: Int -> Int -> Int``` but later we are calling it
with type ```myAdd :: Float -> Float -> Float```.

In contrast with dynamically typed languages ***all the types composed together
by function application have to match up. If they don't, the program will be
rejected by the compiler***.

Maybe create a set of "default rules" to follow? but this is not a simple
scripting language. A defaulting mechanism exists for the ```Num``` class and
it's considered a wart on the language that is almost exclusively used for GHCi.
In other words, Haskell targets [safety first, not usability first](https://www.youtube.com/watch?v=iSmkqocn0oQ).

Now we can write ourselves the most polymorphic type possible for ```myAdd``` or
call the class member function ```add``` directly and everything will work as
expected because there's no type ambiguity:

```haskell
main :: IO ()
main = do
        print "Hello again again!"
        print (add (1::Int) (3::Int))
        print (myAdd (1::Float) (3::Float))

myAdd :: Addition a => a -> a -> a
myAdd = add
```

```haskell
ghci> main
"Hello again again!"
4
4.0
```

### Let-Bound Polymorphism

Suppose we create a very naïve test for our ```Addition``` implementations. Zero
plus some number above zero must always be above zero, so we test this for
```Int``` and ```Float``` at the same time.

```haskell
main :: IO ()
main = do
        print "Testing!"
        let test addFunction =
                if
                      addFunction (0::Int)   (1::Int) > 0
                   && addFunction (0::Float) (1::Float) > 0
                        then "It works!"
                        else "Something is wrong!"
        print "Testing built-in (+)"
        print $ test (+)
        print "Testing our Addition class"
        print $ test add
```

```haskell
> ghci -XHaskell2010 src/TypeCheckingAndInference.hs
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/TypeCheckingAndInference.hs, interpreted )

src/TypeCheckingAndInference.hs:12:36: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Float’
    • In the first argument of ‘addFunction’, namely ‘(0 :: Float)’
      In the first argument of ‘(>)’, namely
        ‘addFunction (0 :: Float) (1 :: Float)’
      In the second argument of ‘(&&)’, namely
        ‘addFunction (0 :: Float) (1 :: Float) > 0’
   |
12 |                    && addFunction (0::Float) (1::Float) > 0
   |                                    ^^^^^^^^

src/TypeCheckingAndInference.hs:12:47: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Float’
    • In the second argument of ‘addFunction’, namely ‘(1 :: Float)’
      In the first argument of ‘(>)’, namely
        ‘addFunction (0 :: Float) (1 :: Float)’
      In the second argument of ‘(&&)’, namely
        ‘addFunction (0 :: Float) (1 :: Float) > 0’
   |
12 |                    && addFunction (0::Float) (1::Float) > 0
   |                                               ^^^^^^^^
Failed, no modules loaded.
```

Sorry to tell you that in any language using the Hindley-Milner type system a
bound function cannot be instantiated in two different ways and here
```addFunction``` is used inside the lambda abstraction in two different ways,
first with type ```Int -> Int -> Int``` and then with type
```Float -> Float -> Float```.

Even if you type annotate ```addFunction``` inside the lambda expression to be
of a the desired polymorphic type like ```Addition a => a -> a -> a``` it will
still fail. It's just a limitation of identifiers bound using a let or where
clause (or at the top level of a module).

### The Monomorphism Restriction

The monomorphism restriction is a generalization to not only lambda expression
binds of the Let-Bound Polymorphism explained above. Haskell places certain
extra restrictions on the generalization step.

> The monomorphism restriction says that any identifier bound by a pattern
> binding (which includes bindings to a single identifier), and having no
> explicit type signature, must be monomorphic. An identifier is monomorphic if
> is either not overloaded, or is overloaded but is used in at most one specific
> overloading and is not exported.
>
> [Gentle Introduction To Haskell, version 98. Revised June, 2000 - 12. Typing Pitfalls](https://www.haskell.org/tutorial/pitfalls.html)

Violations of the monomorphism restriction result in a static type error.

```haskell
mySum = foldl (+) 0
```

```haskell
> ghc src/MonomorphismRestriction.hs
[1 of 1] Compiling Main             ( src/MonomorphismRestriction.hs, src/MonomorphismRestriction.o )

src/MonomorphismRestriction.hs:7:9: error:
    • Ambiguous type variable ‘t0’ arising from a use of ‘foldl’
      prevents the constraint ‘(Foldable t0)’ from being solved.
      Relevant bindings include
        mySum :: t0 Integer -> Integer
          (bound at src/MonomorphismRestriction.hs:7:1)
      Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instances exist:
        instance Foldable (Either a) -- Defined in ‘Data.Foldable’
        instance Foldable Maybe -- Defined in ‘Data.Foldable’
        instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
        ...plus two others
        ...plus 26 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the expression: foldl (+) 0
      In an equation for ‘mySum’: mySum = foldl (+) 0
  |
7 | mySum = foldl (+) 0
  |
```

Solved adding a type-signature expression:

```haskell
mySum :: Num a => [a] -> a
mySum = foldl (+) 0
```

And also solved if, without a type-signature expression, you write the function
this way:

```haskell
mySum xs = foldl (+) 0 xs
```

Why? Because the first one is desugared to ```\f -> foldl (+) 0 f``` while this
one in inferred as ```mySum :: forall {t :: * -> *} {a}. (Foldable t, Num a) => t a -> a```

Don't try this with GHCi because it uses by default an extension called
```NoMonomorphismRestriction``` that tries to infer a type from a list of
default possibilities. The restriction is turned on by default in compiled
modules, and turned off by default at the GHCi prompt (since GHC 7.8.1).

#### Motivation

It is commonly called "The Dreaded Monomorphism Restriction".

It solves two problems:
1. Some ambiguous types (As explained above in Let-Bound Polymorphism).
2. Some repeated evaluation (sharing).

Some thinks this cases are so rare that the restriction is not worth it, some
think this cases should be properly treated (whatever this means).

```haskell
f xs = (len,len)
        where
                len = genericLength xs
```

Without this restriction ```genericLength xs``` may be computed twice, once
for each overloading. Because if the compiler doesn't know the type of the ```(a,b)```
expression, it can't know if ```a``` and ```b``` are the same type.

# Further reading

- [Haskell 2010 Report - 4.3.4 Ambiguous Types, and Defaults for Overloaded Numeric Operations](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4).
- [Gentle Introduction To Haskell, version 98. Revised June, 2000 by Reuben Thomas](https://www.haskell.org/tutorial/index.html).
  - [12 - Typing Pitfalls](https://www.haskell.org/tutorial/pitfalls.html).
- [GHC Docs - 3.4.8. Type defaulting in GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html#extension-ExtendedDefaultRules).
- [Haskell 2010 Report - 4.3.4 Ambiguous Types, and Defaults for Overloaded Numeric Operations](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4).
- The Hindley/Milner type system:
  - A Hindley–Milner (HM) type system is a classical type system for the lambda calculus with parametric polymorphism.
    - [Wikipedia article](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).
  - R. Hindley. The principal type-scheme of an object in combinatory logic. Trans-
actions of the American Mathematical Society, 146:29–60, December 1969.
  - R. Milner. A theory of type polymorphism in programming. Journal of Computer
and System Sciences, 17(3), 1978.
  - L. Damas and R. Milner. Principal type schemes for functional programs. In 9th Annual ACM Symposium on Principles of Programming languages, pages 207–212, Albuquerque, N.M., January 1982.
  - [Write You a Haskell - Hindley-Milner Inference](http://dev.stephendiehl.com/fun/006_hindley_milner.html)
- [GHC Docs - 6.8.4. Default method signatures](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/default_signatures.html).
- [Haskell 2010 Report - 4.5.5 The Monomorphism Restriction](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5)
- [https://wiki.haskell.org/Monomorphism_restriction](https://wiki.haskell.org/Monomorphism_restriction)
- [GHC Docs - 6.12.2. Let-generalisation](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/let_generalisation.html).
- [https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html](https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html)
- [Type Classes with Functional Dependencies, Mark P. Jones, In Proceedings of the 9th European Symposium on Programming, ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html).