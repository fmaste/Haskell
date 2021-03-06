# Monads

## Why? The problem

> We begin with an apparently fundamental conflict. A purely functional program implements a function; it has no side effect. Yet the ultimate purpose of running a program is invariably to cause some side effect: a changed file, some new pixels on the screen, a message sent, or whatever. Indeed it’s a bit cheeky to call input/output “awkward” at all. I/O is the raison d’ˆetre of every program. — a program that had no observable effect whatsoever (no input, no output) would not be very useful.
>
> [Tackling the awkward squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf)

## Haskell's Monad class definition

Remember ```Functor``` and its primary operator ```<$>``` from [here](ApplicativeFunctor.md)?
```haskell
class Functor f where
        fmap :: (a -> b) -> f a -> f b
        (<$) :: a -> f b -> f a
```

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

And ```Applicative``` and its primary function/operator ```<*>``` also from [here](ApplicativeFunctor.md)?

```haskell
class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
```

The Monad class defines the basic operations over a monad, a concept from a branch of mathematics known as [category theory](https://en.wikipedia.org/wiki/Monad_(category_theory)). From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions.

Without further introduction this is a monad in Haskell, just a beefed up applicative functor:
```haskell
class Applicative m => Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        (>>) :: m a -> m b -> m b
        return :: a -> m a 
```

The ```>>=``` function implements sequential composition, is a combinator used to combine/sequence/compose monadic actions and is often pronounced `bind`.

## Historic note

For a long type ```Applicative``` was not a superclass of ```Monad``` but thankfully this was fixed with the [AMP proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal). If you see code not making use of this much needed fix, please understand the past situation!

## Wait, I'm slow

Compare the main composition functions of ```Functor```, ```Applicative``` and ```Monad```

```haskell
(<&>) :: Functor m => m a -> (a -> b) -> m b -- Flipped version of Functor's <$>
(<**>) :: Applicative m => m a -> m (a -> b) -> m b -- A variant of <*> with the arguments reversed.
(>>=) :: Monad m => m a -> (a -> m b) -> m b -- Monadic bind
```

The ```Functor``` type class describes additional context around values using some type and the ```Applicative``` class let's us compose ```Functor```s.

The monadic bind ```>>=``` is a stricter version of this two:
- The monadic context is never duplicated or thrown away no matter what code the programmer writes.
- The only operation that can combine or compose monadic actions is ```>>=```. Treats the monadic context in a single-threaded way.

With Functor this extra structure is often thought of as a "container", while with Monad it tends to be thought of as "side effects".

TODO: `The distinctive feature of Monad compared to other Functors is that it can embed control flow into the extra structure. The reason it can do this is that, unlike fmap which applies a single flat function over the entire structure, (>>=) inspects individual elements and builds new structure from that.`
https://stackoverflow.com/questions/3382210/monad-join-function

TODO: `A Monad is something that "computes" when monadic context is collapsed by join :: m (m a) -> m a (recalling that >>= can be defined as x >>= y = join (fmap y x)). This is how Monads carry context through a sequential chain of computations: because at each point in the series, the context from the previous call is collapsed with the next.`
https://stackoverflow.com/questions/13352205/what-are-free-monads

## Theory

Monads were originally invented in a branch of mathematics called category theory, which is increasingly being applied to describe the semantics of programming languages.

The usefullness of monads to describe composable “computations” was first described by [Eugenio Moggi](https://en.wikipedia.org/wiki/Eugenio_Moggi) in [Computational lambda-calculus and monads](https://person.dibris.unige.it/moggi-eugenio/ftp/lics89.pdf) and also later wrote about how to to structure programs with monads in [Notions of computation and monads](https://person.dibris.unige.it/moggi-eugenio/ftp/ic91.pdf).
More from him at his [personal page](https://person.dibris.unige.it/moggi-eugenio/).

Those are not an easy read at all and are not specific about programming, [Phil Wadler](https://en.wikipedia.org/wiki/Philip_Wadler) in [Comprehending Monads](https://ncatlab.org/nlab/files/WadlerMonads.pdf) describes the usefulness of monads in a programming context.
He wrote several more papers like:
- [The essence of functional programming](https://homepages.inf.ed.ac.uk/wadler/topics/monads.html).
  - This paper explores the use monads to structure functional programs. No prior knowledge of monads or category theory is required.
- [Monads for functional programming](https://homepages.inf.ed.ac.uk/wadler/topics/monads.html).
  - The use of monads to structure functional programs is described.
- [How to declare an imperative](https://homepages.inf.ed.ac.uk/wadler/topics/monads.html)
  - This tutorial describes the use of a monad to integrate interaction into a purely declarative language.
- [Imperative functional programming](https://www.microsoft.com/en-us/research/publication/imperative-functional-programming/) with Simon Peyton Jones.
  - POPL 2003: ten-year most-influential paper award.

More from him at his [personal page](https://homepages.inf.ed.ac.uk/wadler/) and all his monad related publications at this [subpage](https://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

# TODO:

```haskell
class Monad m => MonadFail m where
        fail :: String -> m a
```

# TODO: unsafePerformIO !!!

# TODO

[Arrows, like Monads, are Monoids](https://homepages.inf.ed.ac.uk/cheunen/publications/2006/arrows/arrows.pdf)
