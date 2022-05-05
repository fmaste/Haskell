# Free Monads

## Recursive schemes

Let's remember the classic Haskell recursive [```List```](MonadExample.md) data 
type:
```haskell
data List a = Nil | Cons a (List a)
```

Where its kind and the types of the constructors are (remember the former is a 
type and the latter are expressions/functions that return a value):
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

Someone found a way to factor out the recursion from data types

> Using higher-order functions to implement those recursion schemes makes your
> code clearer, faster, and safer.
>
> [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes):
> Representing common recursion patterns as higher-order functions

I don't who the author(s) of the original idea is(are), we are just explaining
how it works and how it can be useful.

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
