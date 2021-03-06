# Free Monads

## Recursive schemes

```haskell
fix :: (a -> a) -> a
fix f = let {x = f x} in x
```

To the infinity and beyond ```x = f x = f (f x) = f (f (f ( ... )))```

Thank you [lazy evaluation](EvaluationStrategies.md)!

### Classic Recursive List

Let's remember the [classic Haskell recursive ```List```](MonadExample.md) data 
type:
```haskell
data List a = Nil | Cons a (List a)
```

Where the kind of the type constructor ```List``` and the types of the ```Nil```
and ```Cons``` data constructors are:
```haskell
ghci> :k List
List :: * -> *
ghci> :t Nil
Nil :: List a
ghci> :t Cons
Cons :: a -> List a -> List a
```

Remember ```List``` is a ***type constructor***, a function at the type level
that receives types and returns a newly created type to be used by the
compiler's
[type checking and type inference system](TypeCheckingAndInference.md) and
```Nil``` and ```Cons``` are ***data constructors***, functions at the
term level that receive values and return a value to be combined with other
term/value expressions of your codebase.

```List```, the type, is a type constructor that has only one type variable,
```a```. This makes ```List``` a function over types that receives a type and
returns a type, hence kind ```* -> *```.

The other two are data constructors. ```Nil``` has no parameters so it just
returns a value of type ```List a``` while ```Cons```  receives two parameters
of type ```a``` and ```List a``` receptively and returns also a value of type
```List a```.

And the type of an example list using ```List``` is:
```haskell
ghci> :t Cons 'a' ((Cons 'b') (Cons 'c' Nil))
... :: List Char
```

### Recursion Out

Someone found a way to factor out the recursion from data types.

> Using higher-order functions to implement those recursion schemes makes your
> code clearer, faster, and safer.
>
> [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes):
> Representing common recursion patterns as higher-order functions

I don't who the author(s) of the original idea is(are), we just follow the steps
of factoring out the recursive part of a data type to know how it works and
later explain how it can be useful.

Let's ***take out the recursive part with a new type variable ```f```***:
```haskell
data ListF a f = NilF | ConsF a f
```

We have abstracted the recursive part, taking out the "repeated" ```(List a)```
from ```Cons```.
<!-- If ```Cons``` goes here in a newline it's not rendered. -->

Its kind and types are as follows:
```haskell
ghci> :k ListF
ListF :: * -> * -> *
ghci> :t NilF
NilF :: ListF a f
ghci> :t ConsF
ConsF :: a -> f -> ListF a f
```

```ListF``` receives one more type as parameter ```f``` than ```List``` above.

Kinds have changed like this:
- ```List  :: * -> *```
- ```ListF :: * -> * -> *```

And ```ConsF``` instead of receiving a ```List a``` now receives any type as
defined by type variable ```f```.

Types have changed like this:
- ```Cons  :: a -> List a -> List a```
- ```ConsF :: a -> f      -> ListF a f```

Now this new ```ListF``` type makes no sense at all used alone to represent a
list, I can make an instance of type ```ListF``` like ```ListF Int Char```, a
list that holds either one ```Int``` or one ```Int``` plus one ```Char```.

The idea is to create list like value using ```ListF``` as shown below:
```haskell
ghci> :t ConsF 'a' (ConsF 'b' (ConsF 'c' NilF))
... :: ListF Char (ListF Char (ListF Char (ListF a f)))
```

But for our example list we went from type ```List Char``` to
```ListF Char (ListF Char (ListF Char (ListF a f)))```.

We don't know you but we don't want to write that recursive type to infinity
whenever a value of type ```ListF``` is used. Because ```f``` in ```ListF a f```
is a type parameter and it can be anything whenever you use ```ConsF```, like a
list of ```Char``` and one ```Int``` at the end. The
[type inference system](TypeCheckingAndInference.md) can't read your mind to
know what you are unambiguously trying to do:
```haskell
ConsF 'a' (ConsF 'b' (ConsF (1::Int) NilF))
  :: ListF Char (ListF Char (ListF Int (ListF a f)))
```

### Fix It

The proposed solution is to use a type ```Recursive``` defined as follows:
```haskell
data Recursive f = Fix (f (Recursive f))
```

\* The common style is naming it ```data Fix f = Fix (f (Fix f))```, but using
the same name for different things (type constructor and data constructor) even
thou it's valid Haskell syntax and it may be easy for the compiler, it's not
easy for the  untrained eye (I consider this not-appropriate coding style).

Its kind and types are:
```haskell
ghci> :k Recursive
Recursive :: (* -> *) -> *
ghci> :t Fix
Fix :: f (Recursive f) -> Recursive f
```

```Recursive``` is a type function that receives only one type variable ```f```,
with ```f``` being another type function that receives a type and returns
another type.

For example if you apply the type ```Maybe``` to ```Recursive``` you get a new
type. See this useless example to be use as values just to get an idea of how to
"code" at the type level:
```haskell
ghci> :k Recursive
Recursive :: (* -> *) -> *
ghci> :k Maybe
Maybe :: * -> *
ghci> :k Recursive Maybe
Recursive Maybe :: *
ghci> :t Fix Nothing
Fix Nothing :: Recursive Maybe
```

***What we are saying with ```f (Recursive f)``` is that ```f``` is a type
function that receives a ```Recursive f``` as its only parameter. So the value
inside ```Fix``` has to be of the type returned by ```f (Recursive f)```.***

And what type is ```Recursive f```? the type we are defining now. The type that
only has values returned by function ```Fix :: (f (Recursive f)) -> Recursive f```

#### Using Fix

How do we create our example value with our new list type:
```haskell
Fix (ConsF 'a' (Fix (ConsF 'b' (Fix (ConsF 'c' (Fix NilF))))))
```

Writing the list gets more tiresome but the type of the list is easier now:
```haskell
ghci> :t Fix (ConsF 'a' (Fix (ConsF 'b' (Fix (ConsF 'c' (Fix NilF))))))
... :: Recursive (ListF Char)
```

Saying ```Recursive (ListF a)``` is equivalent to ```List a```

And write the list type using ```Fix``` with helper functions:
```haskell
type List a = Recursive (ListF a)

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons a xs = Fix (ConsF a xs)
```

Now our example list and its type is:
```haskell
ghci> :t cons 'a' (cons 'b' (cons 'c' nil))
... :: Recursive (ListF Char)
```

And the list of ```Char``` and one ```Int``` is a type error:
```haskell
ghci> :t cons 'a' (cons 'b' (cons (1::Int) nil))

<interactive>:1:27: error:
    ??? Couldn't match expected type ???Char??? with actual type ???Int???
    ??? In the first argument of ???cons???, namely ???(1 :: Int)???
      In the second argument of ???cons???, namely ???(cons (1 :: Int) nil)???
      In the second argument of ???cons???, namely
        ???(cons 'b' (cons (1 :: Int) nil))???
```

# Further Reading

- https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
- Recursion Schemes
- - https://hackage.haskell.org/package/recursion-schemes
- - https://hackage.haskell.org/package/data-fix
- - https://devtut.github.io/haskell/recursion-schemes.html#recursion-schemes
- - https://medium.com/@jaredtobin/practical-recursion-schemes-c10648ec1c29#.nlgrc4ibh
- AST / Interpreters
  - https://chrispenner.ca/posts/asts-with-fix-and-free
- Free Monads
- - https://devtut.github.io/haskell/free-monads.html
- - https://serokell.io/blog/introduction-to-free-monads
