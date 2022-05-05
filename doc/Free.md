# Free Monads

## Recursive schemes

### Classic Recursive List

Let's remember the [classic Haskell recursive ```List```](MonadExample.md) data 
type:
```haskell
data List a = Nil | Cons a (List a)
```

Where its kind and the types of the constructors are (remember the former is a
type function that returns a type and the latter are value functions that return
 a value):
```haskell
Prelude> :k List
List :: * -> *
Prelude> :t Nil
Nil :: List a
Prelude> :t Cons
Cons :: a -> List a -> List a
```

```List```, the type, is a type function that has only one type  variable,
```a```. This makes ```List``` a function over types that receives a type and 
returns a type, hence kind ```* -> *```.

The other two are value functions, ```Nil``` with no parameters and ```Cons``` 
that receives two parameters of type ```a``` and ```List a``` and returns a
value of type ```List a```.

And the type of an example list using ```List``` is:
```haskell
Prelude> :t Cons 'a' ((Cons 'b') (Cons 'c' Nil))
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

Let's take out the recursive part with a new type variable ```f```:
```haskell
data ListF a f = NilF | ConsF a f
```

We abstracted the recursive part, taking out the repeated ```(List a)``` from 
```Cons```.

And its kind and types are now:
```haskell
Prelude> :k ListF
ListF :: * -> * -> *
Prelude> :t NilF
NilF :: ListF a f
Prelude> :t ConsF
ConsF :: a -> f -> ListF a f
```

Now ```ListF``` receives one more type ```f```:
- ```List  :: * -> *```
- ```ListF :: * -> * -> *```

And ```ConsF``` instead of receiving a ```List a``` receives and ```f```:
- ```Cons  :: a -> List a -> List a```
- ```ConsF :: a -> f      -> ListF a f```

But what is the data type of an example list using ```ListF```:
```haskell
Prelude> :t ConsF 'a' (ConsF 'b' (ConsF 'c' NilF))
... :: ListF Char (ListF Char (ListF Char (ListF a f)))
```

We went from ```List Char``` to
```ListF Char (ListF Char (ListF Char (ListF a f)))```.
We don't know you but we don't want to write that recursive type to infinity 
whenever a value of type ```ListF``` is used, because ```f``` in ```ListF a f```
is a type parameter and it can be anything, like a list of ```Char``` and one
```Int``` at the end:
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
the same name for different things (type and constructor) even thou it's valid
Haskell syntax it may be easy for the compiler but not for the untrained eye (I
consider this not-appropriate coding style).

Its kind and types are:
```haskell
Prelude> :k Recursive
Recursive :: (* -> *) -> *
Prelude> :t Fix
Fix :: f (Fix f) -> Fix f
```

```Recursive``` is a type function that receives only one type variable ```f```,
with ```f``` being another type function that receives a type and returns
another type.

For example if you apply the type ```Maybe``` to ```Recursive``` you get a new
type:
```haskell
Prelude> :k Recursive
Recursive :: (* -> *) -> *
Prelude> :k Maybe
Maybe :: * -> *
Prelude> :k Recursive Maybe
Recursive Maybe :: *
Prelude> :t Fix Nothing
Fix Nothing :: Recursive Maybe
```

Writing the list gets more tiresome but the type of the list is easier now:
```haskell
Prelude> :t Fix (ConsF 'a' (Fix (ConsF 'b' (Fix (ConsF 'c' (Fix NilF))))))
... :: Recursive (ListF Char)
```

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
Prelude> :t cons 'a' (cons 'b' (cons 'c' nil))
... :: Recursive (ListF Char)
```

And the list of ```Char``` and one ```Int``` is a type error:
```haskell
Prelude> :t cons 'a' (cons 'b' (cons (1::Int) nil))

<interactive>:1:21: error:
    • Couldn't match type ‘Int’ with ‘Char’
      Expected type: Recursive (ListF Char)
        Actual type: Recursive (ListF Int)
    • In the second argument of ‘cons’, namely ‘(cons (1 :: Int) nil)’
      In the second argument of ‘cons’, namely
        ‘(cons 'b' (cons (1 :: Int) nil))’
      In the expression: cons 'a' (cons 'b' (cons (1 :: Int) nil))
```

# Further Reading

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
