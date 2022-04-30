# Evaluation Strategies

The evaluation strategy determines when to evaluate the arguments of a function.

Not to be confused with [Lambda Calculus](Lambda.md) reduction strategies, the
process by which a more complex expression is reduced to a simpler expression.

Evaluation strategies combined with the binding strategies that determine what
kind of value to pass to the function, form the ***parameter passing
techniques*** as classified by Erik Crank and Matthias Felleisen in
[Parameter-passing and the lambda calculus](https://doi.org/10.1145/99583.99616).

The choice of a parameter-passing technique is an important element in the
design of a high-level programming language.

```haskell
Prelude> length [1,(1/0),3,4,5]
5
Prelude> take 5 [1,2..]
[1,2,3,4,5]
```

## A Little Terminology

The parameters are ```a``` and ```b``` and the arguments are ```1``` and ```2```:
```haskell
add :: Int -> Int -> Int
add a b = a + b

main :: IO ()
main = print (add 1 2)
```

## Confluence

Thanks first to the work of
[Church and Rosser](https://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-1501858-0/)
and later [Plotkin](https://doi.org/10.1016/0304-3975(75)90017-1) and
[Crank and Felleisen](https://doi.org/10.1145/99583.99616) we know that for any
two reduction and evaluation paths taken, both will evaluate to the same
expression (minus non-termination).

Non-termination is key. In imperative languages the difference between a
call-by-value strategy and a call-by-name strategy can produce different results
for the same program, whereas in functional languages the only observable
difference is its termination behavior.

Wonder why Haskell/GHC tries to stay true to the theory?
See [Type checker and type inference in action](doc/TypeCheckingAndInference.md)

## Strictness

[Lambda Calculus](Lambda.md) has two prevailing evaluation strategies:
- ***Strict***/eager/greedy:
  - All arguments are evaluated before entering the body of a function.
  - If any subexpression fails to have a value, the whole expression fails.
- ***Non-strict***:
  - The arguments are evaluated only when required.
  - Expressions can have a value even if some of their subexpressions do not.

If you have this example function shown below:

```haskell
const :: a -> b -> a
const a b = a
```
Calling ```const (1+1) (1/0)``` will return ```2``` using non-strict evaluation
and ```error``` using strict evaluation.

### Binding Strategies

- [Strict](https://en.wikipedia.org/wiki/Evaluation_strategy#Strict_binding_strategies)
  - Call-by-value
    - The evaluated value of the argument expression is bound to the corresponding variable in the function.
  - Call-by-reference (or pass by reference)
    - A parameter is bound to a reference to the variable used as argument, rather than a copy of its value.
- [Non-strict](https://en.wikipedia.org/wiki/Evaluation_strategy#Non-strict_binding_strategies)
  - Call-by-name
    - Arguments are substituted directly into the function body.
  - Call-by-need
    - Call-by-name with [memoization](https://en.wikipedia.org/wiki/Memoization), if the function argument is evaluated its value is stored for subsequent uses. So the expression is evaluated no more than once.

## Haskell's definition

> Function application in Haskell is ***non-strict***; that is, a function
> argument is evaluated only when required. Sometimes it is desirable to force
> the evaluation of a value, using the ```seq``` function:
>
>  ```seq :: a -> b -> b```
>
> The function ```seq``` is defined by the equations:
>
> ```seq ⊥ b  =  ⊥```
>
> ```seq a b  =  b, if a ≠ ⊥```
>
> ```seq``` is usually introduced to improve performance by avoiding unneeded
> laziness. Strict datatypes
> (see Section [4.2.1](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-710004.2.1))
> are defined in terms of the ```$!``` operator. However, the provision of
> ```seq``` has important semantic consequences, because it is available at
> every type. As a consequence, ```⊥``` is not the same as ```\x -> ⊥```, since
> ```seq``` can be used to distinguish them. For the same reason, the existence
> of ```seq``` weakens Haskell’s parametricity properties.
>
> The operator ```$!``` is strict (call-by-value) application, and is defined in
> terms of ```seq```. The Prelude also defines the ```$``` operator to perform
> non-strict application.
>
> ```infixr 0 $, $!```
>
> ```($), ($!) :: (a -> b) -> a -> b```
>
> ```f $  x   =          f x```
>
> ```f $! x   =  x ‘seq‘ f x```
>
> The non-strict application operator ```$``` may appear redundant, since
> ordinary application ```(f x)``` means the same as ```(f $ x)```. However,
> ```$``` has low, right-associative binding precedence, so it sometimes allows
> parentheses to be omitted; for example:
>
> ```f $ g $ h x  =  f (g (h x))```
>
> It is also useful in higher-order situations, such as ```map ($ 0) xs```, or
> ```zipWith ($) fs xs```.

[Haskell 2010 report - 6.2 Strict Evaluation](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1260006.2).

## [Lazy vs. non-strict](https://wiki.haskell.org/Lazy_vs._non-strict)

Non-strictness is often confused with lazy evaluation.

Haskell is often described as a lazy language. However, the language specification simply states that Haskell is ***[non-strict](https://wiki.haskell.org/Non-strict_semantics)***, which is not quite the same thing as ***[lazy](https://wiki.haskell.org/Lazy_evaluation)***.

Lazy evaluation is classified as a binding technique rather than an evaluation strategy.

### Note

A Haskell implementation using call-by-name, would be technically conforming?

# Memoization

Now that we know that Haskell computes any given expression at most once every
time the lambda expression is entered, we can use the memoization optimization
technique:

```haskell
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)
```

```haskell
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
        where fib 0 = 0
              fib 1 = 1
              fib n = memoized_fib (n-2) + memoized_fib (n-1)
```

```haskell
memoized_fib' :: Int -> Integer
memoized_fib' = \i -> (map fib [0 ..] !!) i
        where fib 0 = 0
              fib 1 = 1
              fib n = memoized_fib (n-2) + memoized_fib (n-1)
```

The hard part is knowing where the lambda expressions are.

# Further Reading

- [The Haskell 2010 Report (PDF)](https://www.haskell.org/definition/haskell2010.pdf)
- 

https://bor0.wordpress.com/2020/12/11/haskell-memoization-and-evaluation-model/

[3.1 Errors](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-230003.1)

 Errors during expression evaluation, denoted by ⊥ (“bottom”), are indistinguishable by a Haskell program from non-termination. Since Haskell is a non-strict language, all Haskell types include ⊥. That is, a value of any type may be bound to a computation that, when demanded, results in an error. When evaluated, errors cause immediate program termination and cannot be caught by the user. The Prelude provides two functions to directly cause such errors:
error     :: String -> a  
undefined :: a

A call to error terminates execution of the program and returns an appropriate error indication to the operating system. It should also display the string in some system-dependent manner. When undefined is used, the error message is created by the compiler.

Translations of Haskell expressions use error and undefined to explicitly indicate where execution time errors may occur. The actual program behavior when an error occurs is up to the implementation. The messages passed to the error function in these translations are only suggestions; implementations may choose to display more or less information when an error occurs.

TODO: Look for this papers:

Hughes 1984 argues for lazy evaluation as a mechanism for improving program modularity through separation of concerns, by easing independent implementation of producers and consumers of data streams. Launchbury 1993 describes some difficulties that lazy evaluation introduces, particularly in analyzing a program's storage requirements, and proposes an operational semantics to aid in such analysis. Harper 2009 proposes including both strict and lazy evaluation in the same language, using the language's type system to distinguish them.

- [Church, A., Rosser, J. B.: Some Properties of Conversion, Trans. Amer. Math. Soc., 39, 1936, 472–482](https://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-1501858-0/)
  - [PDF](https://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-1501858-0/S0002-9947-1936-1501858-0.pdf)
- [Dexter Kozen. 2010. Church-Rosser Made Easy. Fundam. Inf. 103, 1–4 (January 2010), 129–136.](https://dl.acm.org/doi/abs/10.5555/1922521.1922529)
  - [PDF]()
- [Church–Rosser Theorem - Wikipedia](https://en.wikipedia.org/wiki/Church%E2%80%93Rosser_theorem)
- [Erik Crank and Matthias Felleisen. 1991. Parameter-passing and the lambda calculus. In Proceedings of the 18th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '91). Association for Computing Machinery, New York, NY, USA, 233–244.](https://doi.org/10.1145/99583.99616)
  - [PDF](https://dl.acm.org/doi/pdf/10.1145/99583.99616)
- [G.D. Plotkin, Call-by-name, call-by-value and the λ-calculus, Theoretical Computer Science, Volume 1, Issue 2, December 1975, Pages 125-159](https://doi.org/10.1016/0304-3975(75)90017-1)
  - [PDF](https://homepages.inf.ed.ac.uk/gdp/publications/cbn_cbv_lambda.pdf)
- [Evaluation Strategy - Wikipedia](https://en.wikipedia.org/w/index.php?title=Evaluation_strategy&oldid=681333382)
- [Reduction Strategy - Wikipedia](https://en.wikipedia.org/w/index.php?title=Reduction_strategy_%28lambda_calculus%29&oldid=639577658)
- [What's the difference between reduction strategies and evaluation strategies? - StackExchange](https://cstheory.stackexchange.com/questions/32551/whats-the-difference-between-reduction-strategies-and-evaluation-strategies)
- [Differences Between Parameters and Arguments](https://developer.mozilla.org/en-US/docs/Glossary/Parameter)
