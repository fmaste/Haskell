# Lambda calculus

The subarea of
[Theoretical Computer Science](https://en.wikipedia.org/wiki/Theoretical_computer_science) called [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (also
written as λ-calculus) is a formal system developed by
[Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) and
[Stephen Kleene](https://en.wikipedia.org/wiki/Stephen_Cole_Kleene) to study
[Computability](https://en.wikipedia.org/wiki/Computability). Probably the
highest level theoretical/formal model of computation that will ever be
conceived.

Like any [formal system](https://en.wikipedia.org/wiki/Formal_system), it's an
abstract structure used for inferring theorems from axioms according to a set of
rules. It's a set of [Axioms](https://en.wikipedia.org/wiki/Axiom) (something
taken to be true as a starting point for further reasoning).

There are other
[models of computation](https://en.wikipedia.org/wiki/Model_of_computation), the
best known is probably the [finite state machine](https://en.wikipedia.org/wiki/Finite_state_machine) that is a sequential model. Lambda calculus is a
functional model and it is based on ***function abstraction (or creation)*** and
***function application*** using variable binding and substitution.

## Alphabet

The alphabet is the finite set of symbols which concatenate formulas. Or a formula is just a finite string of symbols taken from the alphabet.

The particular list of symbols consists of ```{```, ```}```, ```(```, ```)```, ```λ```, ```[``` ,```]``` and an enumerably infinite set of symbols ```a```, ```b```, ```c```, . . . to be called variables.

## Grammar

A grammar consists of rules to form ***well-formed formula*** from simpler formulas.

The set of well-formed formula lambda expressions or λ-terms, ```Λ```, is defined by induction as follows:

1. If ```x``` is a variable, then ```x ∈ Λ```.
   - The occurrence of ```x``` in it is an occurrence of x as a free variable in it.
2. Application: If ```F, X ∈ Λ```, then ```{F}(X) ∈ Λ```.
   - An occurrence of ```x``` as a free (bound) variable in ```F``` or ```X``` is an occurrence of ```x``` as a free (bound) variable in ```{F}(X)```.
3. Abstraction: If ```x``` is a variable and ```M ∈ Λ```, then ```(λx[M]) ∈ Λ```.
   - If the formula ```M``` contains an occurrence of ```x``` as a free variable in ```M```, any occurrence of ```x``` in ```λx[M]``` is an occurrence of ```x``` as a bound variable in ```λx[M]```, and an occurrence of a variable ```y```, other than ```x```, as a free (bound) variable in ```M``` is an occurrence of ```y``` as a free (bound) variable in ```λx[M]```.

The main idea is forming functions by abstraction and applying functions
parameters to arguments.

### In BNF notation

```varid → (small {small})```

```formula → varid | {formula}(formula) | (λvarid[formula])```

### Abbreviations

- Application:
  - ```{F}(X)``` to ```F(X)```
  - ```{ {F}(X) }(Y)``` to ```{F}(X,Y)``` or ```F(X,Y)```
  - ```{ { {F}(X) }(Y) }(Z)``` to ```{F}(X,Y,Z)``` or ```F(X,Y,Z)```
  - and so on
- Abstraction:
  - ```(λx[M])``` to ```λx[M]```
  - or to ```λx.M``` with the more commonly used ```.``` notation
  - This is a also common abbreviation:
    - ```λx[λy[...λz[M]]]``` to ```λxyz.M```
    - But is the bound variable ```x``` or is it ```xyz```? Try not to use it

Parentheses are used just for grouping; they have no meaning on their own.
Lambda terms are greedy, extending as far to the right as they can. The term
```λx. λy. y``` is the same as ```λx.(x (λy.y))```, not (λx.x) (λy.y).
Application terms are left-associative, so ```x y z``` is the same thing as
```(x y) z```.

### Free variable vs. bound variable

In lambda calculus, ```x``` is a bound variable in the term ```λx.T``` and a free variable in the term ```T```. We say ```x``` is bound in ```λx.T``` and free in ```T```.

In some function a variable is said to be a ***free variable when its value
depends on the context where the function is called***. In contrast, we called a
variable ***a bounded variable when it value does not depend on the context of
the function call***.

If ```M``` has no free variables ```M``` is called a ***combinator***.

## Theorems

TODO:

Three theorems of lambda calculus are
- beta-conversion
- alpha-conversion
- eta-conversion
Lambda-reduction (also called lambda conversion) refers to all three

# Types

Since the development of Hindley–Milner type inference in the 1970s, functional programming languages have tended to use typed lambda calculus, rejecting all invalid programs at compilation time and risking false positive errors, as opposed to the untyped lambda calculus, that accepts all valid programs at compilation time and risks false negative errors.

# Further Reading

- In the course of studying the [Entscheidungsproblem](https://en.wikipedia.org/wiki/Entscheidungsproblem) Church gave a formal model of computation in:
  - [A. Church, “An Unsolvable Problem of Elementary Number Systems,” Amer. J. Math. 58 (1936), 345–363](https://www.ics.uci.edu/~lopes/teaching/inf212W12/readings/church.pdf).
    - Provides a rigorous formal characterization of what it means to be [solvable by means of an algorithm](https://en.wikipedia.org/wiki/Computability), known as [Church–Turing thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis).
    - [Want to buy it?](https://www.sophiararebooks.com/pages/books/4543/alonzo-church/an-unsolvable-problem-in-elementary-number-theory)
- The equivalence between the partial recursive functions and the Turing computable functions was shown by Kleene:
  - S. C. Kleene, “General Recursive Functions of Natural Numbers,” Math. Annalen 112 (1936), 727–742.
  - It seems to be incidental that the calculus invented by Church turned out to be Turing complete.
- [General recursive functions / partial recursive function / μ-recursive functions](https://en.wikipedia.org/wiki/General_recursive_function)
  - The smallest class of functions (with arbitrarily many arguments) that is closed under composition, recursion, and minimization, and includes zero, successor, and all projections.
- [History of the Church–Turing thesis](https://en.wikipedia.org/wiki/History_of_the_Church%E2%80%93Turing_thesis)
- [The Mathematica GuideBooks](http://www.mathematicaguidebooks.org/)
- Seldin (2006). The logic of Curry and Church. In Handbook of the History of Logic, vol.5: Logic from Russell to Church, p. 819—874. North-Holland: Amsterdam. 
  - Link [1](https://www.elsevier.com/search-results?labels=books&book-series=Handbook%20of%20the%20History%20of%20Logic&sort=document.published-asc), [2](https://www.sciencedirect.com/handbook/handbook-of-the-history-of-logic) and [3](https://people.uleth.ca/~jonathan.seldin/CCL.pdf).
- http://cs.brown.edu/people/jsavage/book/
  - https://cs.brown.edu/people/jsavage/book/pdfs/ModelsOfComputation.pdf
  - Types: [Church (1940). A formulation of the simple theory of types. Journal of Symbolic Logic 5(2):56—68](www.classes.cs.uchicago.edu/archive/2007/spring/32001-1/papers/church-1940.pdf).
- [Stanford Encyclopedia of Philosophy: The Lambda Calculus](https://plato.stanford.edu/entries/lambda-calculus/)
- [CS 312 Recitation 26 The Lambda Calculus](https://www.cs.cornell.edu/courses/cs3110/2008fa/recitations/rec26.html)
- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
