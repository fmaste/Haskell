# [Haskell](http://www.haskell.org/), an advanced, purely functional programming language

***A mixture of quick reference guide and Haskell tutorial*** [^1] [^2]

[^1]: Based on [GHC](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler), a state-of-the-art, open source, compiler and interactive environment for the functional language Haskell

[^2]: Using [version 9.2.2](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/index.html) of GHC

## Preface

`“Some half dozen persons have written technically on combinatory logic, and most of these, including ourselves, have published something erroneous. Since some of our fellow sinners are among the most careful and competent logicians on the contemporary scene, we regard this as evidence that the subject is refractory. Thus fullness of exposition is necessary for accuracy; and excessive condensation would be false economy here, even more than it is ordinarily.”`

Haskell B. Curry and Robert Feys
in the Preface to Combinatory Logic [\[3\]](https://www.haskell.org/onlinereport/haskell2010/haskellli3.html#Xcurry&feys:book), May 31, 1956

[^3]: H.K. Curry and R. Feys. Combinatory Logic. North-Holland Pub. Co., Amsterdam, 1958

## History
Read [A History of Haskell: Being Lazy with Class](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf) from 2007

From its abstract:
`This paper describes the history of Haskell, including its genesis and principles, technical contributions, implementations and tools, and applications and impact.`

Also [this video](https://www.youtube.com/watch?v=re96UgMk6GQ) from 2017 where Symon Peyton Jones discusses Haskell’s birth and evolution

## Inception

In a meeting held at the conference on Functional Programming Languages and Computer Architecture (FPCA ’87) in Portland, Oregon in September of 1987, it was decided that a committee should be formed to design a much needed common language for a ***purely functional programming languages***

The committee’s primary goal was to design a language that satisfied these constraints:

- It should be suitable for teaching, research, and applications, including building large systems.
- It should be completely described via the publication of a formal syntax and semantics.
- It should be freely available. Anyone should be permitted to implement the language and distribute it to whomever they please.
- It should be based on ideas that enjoy a wide consensus.
- It should reduce unnecessary diversity in functional programming languages.

[This documents](https://wiki.haskell.org/Language_and_library_specification) describes the results of that (and subsequent) committee’s efforts: a purely functional programming language called Haskell, named after the logician Haskell B. Curry whose work provides the logical basis for much of ours.

## Purpose

A [+10 years old funny video](https://www.youtube.com/watch?v=iSmkqocn0oQ) of what Haskell wants to achieve and the path taken to achieve it

## Main Features

### Purely functional

Every function in Haskell is a function in the mathematical sense (i.e., "pure"). Even side-effecting IO operations are but a description of what to do, produced by pure code. There are no statements or instructions, only expressions which cannot mutate variables (local or global) nor access state like time or random numbers.

### Statically typed

Every expression in Haskell has a type which is determined at compile time. All the types composed together by function application have to match up. If they don't, the program will be rejected by the compiler. Types become not only a form of guarantee, but a language for expressing the construction of programs.

### Type inference

You don't have to explicitly write out every type in a Haskell program. Types will be inferred by unifying every type bidirectionally. However, you can write out types if you choose, or ask the compiler to write them for you for handy documentation.

### Lazy

Functions don't evaluate their arguments. This means that programs can compose together very well, with the ability to write control constructs (such as if/else) just by writing normal functions. The purity of Haskell code makes it easy to fuse chains of functions together, allowing for performance benefits.

### Concurrent

Haskell lends itself well to concurrent programming due to its explicit handling of effects. Its flagship compiler, GHC, comes with a high-performance parallel garbage collector and light-weight concurrency library containing a number of useful concurrency primitives and abstractions.

### Packages

Open source contribution to Haskell is very active with a wide range of packages available on the public package servers.

## Prelude

Basic types and clasess that are in scope by default in every Haskell file

See [Prelude](Prelude.md)
