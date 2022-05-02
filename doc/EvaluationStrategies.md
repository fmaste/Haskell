# Evaluation Strategies

[Functional programming languages](https://en.wikipedia.org/wiki/Functional_programming)
extend Church's [Lambda Calculus](Lambda.md) with a variety of constructs (AKA
tons of syntactic sugar) that make it useful for programmers.

The evaluation strategy determines when to evaluate argument expressions during
function application. Although sometimes it looks closer to
[operational semantics](https://en.wikipedia.org/wiki/Operational_semantics)
than
[denotational semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics),
its choice is an important element in the design of a
[high-level programming language](https://en.wikipedia.org/wiki/High-level_programming_language).

```haskell
Prelude> length [1,(1/0),3,4,5]
5
Prelude> take 5 [1,2..]
[1,2,3,4,5]
```

## A Little Terminology

Not to be confused with [λ-Calculus](Lambda.md) reduction strategies, the
process by which a more complex expression is reduced to a simpler expression.

Also notice that we are still not talking about the evaluation order when not
constrained by operator precedence or associativity, that also in languages like
C is unspecified. We are talking about how the expressions that need further
evaluation and are used as function arguments are treated.

By parameters we mean ```a``` and ```b``` and by arguments ```1``` and ```2*3```
in the example below:
```haskell
add :: Int -> Int -> Int
add a b = a + b

main :: IO ()
main = print $ add 1 (2*3)
```

## Parameter-Passing Classification

We use the classification described by Erik Crank and Matthias Felleisen in
[Parameter-passing and the lambda calculus](https://doi.org/10.1145/99583.99616)
where ***evaluation strategies*** are combined with the more specific notion of
***binding strategies*** that determines what kind of value to pass to the
function.

### Strictness

[λ-Calculus](Lambda.md) has two prevailing semantics:

- ***Strict*** or eager (also greedy/applicative order) evaluation:
  - All argument expressions to a function are evaluated before binding the parameters.
  - ***Semantics***: If any subexpression fails to have a value, the whole expression fails.
- ***Non-strict*** (or normal order but not lazy) evaluation:
  - All argument expressions are passed unevaluated to the body of the function.
  - ***Semantics***: Expressions can have a value even if some of their subexpressions do not.

If we have this example function shown below:

```haskell
const :: a -> b -> a
const a b = a
```

Calling ```const (1+1) (1/0)``` will return ```2``` in Haskell and "error" using
strict semantics.

<!-- A function ```f``` is strict if and only if ```f ⊥ = ⊥```. -->

### Binding Strategies

We defined if argument expressions are evaluated before function application or
not, but what is passed as value to the function in those parameters?

- [Strict:](https://en.wikipedia.org/wiki/Evaluation_strategy#Strict_binding_strategies)
  - Call-by-value:
    - The evaluated values of the argument expressions are bound to the corresponding parameters in the function.
  - Call-by-reference:
    - Parameters are bound to a reference to the variable used as argument, rather than a copy of its value.
- [Non-strict:](https://en.wikipedia.org/wiki/Evaluation_strategy#Non-strict_binding_strategies)
  - Call-by-name:
    - Argument expressions are substituted directly into the function body.
  - Call-by-need:
    - Argument expressions are evaluated at most once and, if possible, not at all.

Most authors refer to strict evaluation as call-by-value due to the
call-by-value binding strategy requiring strict evaluation and to call-by-name
as non-strict evaluation because it means that arguments are passed unevaluated
to the function body, which by popular folklore is the usual Church's lambda
calculus strategy.

## [Confluence](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting))

With the works of
[Church and Rosser](https://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-1501858-0/)
about reduction sequences and
[Plotkin](https://doi.org/10.1016/0304-3975(75)90017-1) about parameter-passing
techniques as foundation, we know that two different reduction sequence or
evaluation strategy cannot lead to different computation results (minus
non-termination).

Non-termination is key. In most imperative languages different evaluation
strategies can produce different results for the same program, whereas in
[purely functional languages](https://en.wikipedia.org/wiki/Purely_functional_programming)
the only output difference is its termination behavior.

We need a proper definition of ***purely functional language***:

> A language is purely functional if (i) it includes every simply typed
> λ-calculus term, and (ii) its call-by-name, call-by-need, and call-by-value
> implementations are equivalent (modulo divergence and errors).
>
> [A. M. R. SABRY, “What is a purely functional language?”, Journal of Functional Programming, vol. 8, no. 1, pp. 1–22, 1998.](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.27.7800)

In a purely functional language the function return values are identical for
identical arguments. Obviously call-by-reference introduces side effects.

No wonder why Haskell and GHC do everything possible to stay true to the theory
when extending lambda calculus. See
[Type checker and type inference in action](doc/TypeCheckingAndInference.md)
to get an idea of what it means for the usability of the language to be pure (no
side-effects), non-strict and statically typed.

## [Lazy vs. non-strict](https://wiki.haskell.org/Lazy_vs._non-strict)

> If a function is non-strict, we say that it is lazy. Technically, this is an
> abuse of terminology, since lazy evaluation is an implementation technique
> which implements non-strict semantics. However, ‘lazy’ is such an evocative
> term that it is often used where ‘non-strict’ would be more correct.
>
> [The Implementation of Functional Programming Languages, Simon Peyton Jones, Published by Prentice Hall \| January 1987](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/)

The term lazy is informally interchangeable with non-strict because it's the
prevalent implementation technique for non-strict languages, but laziness is not
the only way to implement non-strictness.

### Haskell's definition

Haskell is often described as a lazy language. However, the language
specification simply states that Haskell is
***[non-strict](https://wiki.haskell.org/Non-strict_semantics)***, which is not
quite the same thing as ***[lazy](https://wiki.haskell.org/Lazy_evaluation)***:

> Function application in Haskell is ***non-strict***; that is, a function
> argument is evaluated only when required.
>
> [Haskell 2010 report - 6.2 Strict Evaluation](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1260006.2).

A Haskell implementation using call-by-name would be technically conforming.

### Memoization

Technically, lazy evaluation means call-by-name with [memoization](https://wiki.haskell.org/Memoization).

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
memoized_fib i = map fib [0 ..] !! i
        where fib 0 = 0
              fib 1 = 1
              fib n = memoized_fib (n-2) + memoized_fib (n-1)
```

### Laziness

Lazy refers to operational behavior and its implementation details are only
summarized here.

The first ingredient of call-by-need, that a function argument is evaluated only
when required, is directly implemented by using a normal order reduction. The
second ingredient, that once evaluated should never be re-evaluated, is
implemented with:
- Pointer substitution.
- Updating the root of the redex tree with the results.

# Further Reading

- [Alonzo Church and J. B. Rosser. Some properties of conversion. Transactions of the American Mathematical Society, vol. 39 (1936), pp. 472–482.](https://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-1501858-0/)
  - [PDF](https://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-1501858-0/S0002-9947-1936-1501858-0.pdf)
- [Dexter Kozen. 2010. Church-Rosser Made Easy. Fundam. Inf. 103, 1–4 (January 2010), 129–136.](https://dl.acm.org/doi/abs/10.5555/1922521.1922529)
  - [PDF]()
- [Church–Rosser Theorem - Wikipedia](https://en.wikipedia.org/wiki/Church%E2%80%93Rosser_theorem)
- [G.D. Plotkin, Call-by-name, call-by-value and the λ-calculus, Theoretical Computer Science, Volume 1, Issue 2, December 1975, Pages 125-159](https://doi.org/10.1016/0304-3975(75)90017-1)
  - [PDF](https://homepages.inf.ed.ac.uk/gdp/publications/cbn_cbv_lambda.pdf)
- [Erik Crank and Matthias Felleisen. 1991. Parameter-passing and the lambda calculus. In Proceedings of the 18th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '91). Association for Computing Machinery, New York, NY, USA, 233–244.](https://doi.org/10.1145/99583.99616)
  - [PDF](https://dl.acm.org/doi/pdf/10.1145/99583.99616)
- [A. M. R. SABRY, “What is a purely functional language?”, Journal of Functional Programming, vol. 8, no. 1, pp. 1–22, 1998.](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.27.7800)
  - [PDF](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/3A39D50DA48F628D17D9A768A1FA39C3/S0956796897002943a.pdf/what-is-a-purely-functional-language.pdf)
- [The Implementation of Functional Programming Languages, Simon Peyton Jones, Published by Prentice Hall \| January 1987](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/)
  - [PDF - Single-page portrait](https://www.microsoft.com/en-us/research/uploads/prod/1987/01/slpj-book-1987.pdf)
  - [PDF - Double-page landscape](https://www.microsoft.com/en-us/research/uploads/prod/1987/01/slpj-book-2up.pdf)
- [Evaluation Strategy - Wikipedia](https://en.wikipedia.org/w/index.php?title=Evaluation_strategy&oldid=681333382)
- [Reduction Strategy - Wikipedia](https://en.wikipedia.org/w/index.php?title=Reduction_strategy_%28lambda_calculus%29&oldid=639577658)
- [What's the difference between reduction strategies and evaluation strategies? - StackExchange](https://cstheory.stackexchange.com/questions/32551/whats-the-difference-between-reduction-strategies-and-evaluation-strategies)
- [Differences Between Parameters and Arguments](https://developer.mozilla.org/en-US/docs/Glossary/Parameter)
- [How does non-strict and lazy differ?](https://exchangetuts.com/how-does-non-strict-and-lazy-differ-1639549927834114)
- [Design Concepts in Programming Languages By Franklyn Turbak and David Gifford](https://mitpress.mit.edu/books/design-concepts-programming-languages)

- [The Haskell 2010 Report (PDF)](https://www.haskell.org/definition/haskell2010.pdf)

<!--
https://bor0.wordpress.com/2020/12/11/haskell-memoization-and-evaluation-model/

[3.1 Errors](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-230003.1)

 Errors during expression evaluation, denoted by ⊥ (“bottom”), are indistinguishable by a Haskell program from non-termination. Since Haskell is a non-strict language, all Haskell types include ⊥. That is, a value of any type may be bound to a computation that, when demanded, results in an error. When evaluated, errors cause immediate program termination and cannot be caught by the user. The Prelude provides two functions to directly cause such errors:
error     :: String -> a  
undefined :: a

A call to error terminates execution of the program and returns an appropriate error indication to the operating system. It should also display the string in some system-dependent manner. When undefined is used, the error message is created by the compiler.

Translations of Haskell expressions use error and undefined to explicitly indicate where execution time errors may occur. The actual program behavior when an error occurs is up to the implementation. The messages passed to the error function in these translations are only suggestions; implementations may choose to display more or less information when an error occurs.

TODO: Look for this papers:

Hughes 1984 argues for lazy evaluation as a mechanism for improving program modularity through separation of concerns, by easing independent implementation of producers and consumers of data streams. Launchbury 1993 describes some difficulties that lazy evaluation introduces, particularly in analyzing a program's storage requirements, and proposes an operational semantics to aid in such analysis. Harper 2009 proposes including both strict and lazy evaluation in the same language, using the language's type system to distinguish them.
-->
