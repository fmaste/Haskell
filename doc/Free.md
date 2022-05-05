# Free Monads

## Recursive schemes

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
Prelude> :t Cons 'a' ((Cons 'b') (Cons 'c' Nil))
Cons 'a' ((Cons 'b') (Cons 'c' Nil)) :: List Char
```

```List```, the type, is a type function that has only one type  variable,
```a```. This makes ```List``` a function over types that receives a type and 
returns a type, hence kind ```* -> *```.

The other two are value functions, ```Nil``` with no parameters and ```Cons``` 
that receives two parameters of type ```a``` and ```List a``` and return a
value of type ```List a```.

Someone found a way to factor out the recursion from data types

> Using higher-order functions to implement those recursion schemes makes your
> code clearer, faster, and safer.
>
> [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes):
> Representing common recursion patterns as higher-order functions

I don't who the author(s) of the original idea is(are), just follow the steps
of factoring out the recursive part of a data type to know how it works and
later explain how it can be useful.

Let's take out the recursive part with a new type variable ```f```:
```haskell
data ListF a f = NilF | ConsF a f
```

We abstracted the recursive part, taking out ```(List a)``` from ```Cons```.

And its kind and type are know:
```haskell
Prelude> :k ListF
ListF :: * -> * -> *
Prelude> :t NilF
NilF :: ListF a f
Prelude> :t ConsF
ConsF :: a -> f -> ListF a f
```

But what is the data type of a list using ```ListF```:
```haskell
Prelude> :t ConsF 'a' (ConsF 'b' (ConsF 'c' NilF))
ConsF 'a' (ConsF 'b' (ConsF 'c' NilF))
  :: ListF Char (ListF Char (ListF Char (ListF a f)))
```

We went from to ```List Char``` to
```ListF Char (ListF Char (ListF Char (ListF a f)))```.
We don't know you but we don't want to write that recursive type to infinity 
whenever a value of type ```ListF``` is used, because ```f``` in ```ListF a f``` 
is a type parameter it can be anything, like a list of ```Char``` and one
```Int``` at the end:
```haskell
ConsF 'a' (ConsF 'b' (ConsF (1::Int) NilF))
  :: ListF Char (ListF Char (ListF Int (ListF a f)))
```

The solution is to write the list type using ```Fix```:
```haskell
data Fix f = Fix (f (Fix f))
```

```Fix``` is a type that receives only one type variable (parameter), a type
function (a function over types that receives a type and returns another type):
```haskell
Prelude> :k Fix
Fix :: (* -> *) -> *
Prelude> :t Fix
Fix :: f (Fix f) -> Fix f
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
