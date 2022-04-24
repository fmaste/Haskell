## Type checking and type inference in action

Define this type class and 2 instance declarations:

```haskell
class Addition a where
        add :: a -> a -> a

instance Addition Int where
        add = (+)

instance Addition Float where
        add a _ = b -- Just for fun
```

If you try to add function ```myAdd``` without any type information as show
below:

```haskell
myAdd = add
```

the compiler must fail:

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

As there's no caller of this function the compiler has no way to know which
implementation is intended to be used, it can't choose one implementation and
hence infer the type of ```myAdd```. Imagine what could happen if it chooses an
unintended implementation, ```1 + 2``` could become ```4```, who knows!

We could say that this was a problem of the type inference system and as with
most type error in Haskell, with proper type annotations it should work:

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

Now before trying to add any type information to the ```myAdd``` function, try
calling it twice with different types like this:

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

The type inference system is good but can't be that good while trying to be
unambiguous. With the first usage parsed it inferred that the type of
```myAdd``` was ```myAdd :: Int -> Int -> Int``` but later we are calling it
with type ```myAdd :: Float -> Float -> Float```.

The same way it can't pick a specific implementation of class ```Addition``` it
can't make function ```myAdd``` use restricted polymorphism / overloading by its
own. How can the compiler be sure that's unambiguously what the developer wants?
This is not a scripting language.

In contrast with dynamically typed languages ***all the types composed together
by function application have to match up. If they don't, the program will be
rejected by the compiler***.

Now we can write ourselves the most abstract type as possible to ```myAdd``` or
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
