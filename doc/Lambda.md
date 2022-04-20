# Lambda calculus

The subarea of [Theoretical Computer Science](https://en.wikipedia.org/wiki/Theoretical_computer_science) called [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (also written as λ-calculus) is a formal system for expressing computations developed by [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) and [Stephen Kleene](https://en.wikipedia.org/wiki/Stephen_Cole_Kleene). Probably the highest level theoretical/formal model of computation that will ever be conceived.

Like any [formal system](https://en.wikipedia.org/wiki/Formal_system), it's an abstract structure used for inferring theorems from axioms according to a set of rules.

It is based on function abstraction and application using variable binding and substitution.

It's a set of [Axioms](https://en.wikipedia.org/wiki/Axiom) (something taken to be true as a starting point for further reasoning) 

## Alphabet

The alphabet is the finite set of symbols which concatenate formulas. Or a formula is just a finite string of symbols taken from the alphabet.

We select a particular list of symbols, consisting of the symbols ```{```, ```}```, ```(```, ```)```, ```λ```, ```[``` ,```]``` and an enumerably infinite set of symbols ```a```, ```b```, ```c```, . . . to be called variables.

## Grammar

A grammar consists of rules to form formulas from simpler formulas.

The main idea is forming functions by abstraction and applying a function to an argument. The set of lambda expressions or λ-terms, ```Λ```, is defined by induction as follows:
1. If ```x``` is a variable, then ```x ∈ Λ```
2. Abstraction (AKA function definition): If ```x``` is a variable and ```M ∈ Λ```, then ```(λx.M) ∈ Λ``` (sometimes shown as ```(λx[M])```).
3. Application (AKA provide a value for a parameter of a function): If ```M, N ∈ Λ```, then ```(M N) ∈ Λ```.
We take M and N as sub expresssions, which can be any of the above forms.

In some function a variable is said to be a ***free variable*** when its value depends on the context where the function is called. In contrast, we called a variable a ***bounded variable*** when it value does not depend on the context of the function call.


In lambda calculus, ```x``` is a bound variable in the term ```M = λx. T``` and a free variable in the term ```T```. We say ```x``` is bound in ```M``` and free in ```T```. If T contains a subterm λx. U then x is rebound in this term. This nested, inner binding of x is said to "shadow" the outer binding. Occurrences of x in U are free occurrences of the new x

The terms `well-formed formula`, `free variable`, and `bound variable` 


-  A formula is said to be well-formed if it can be formed using the rules of the formal grammar. It is often required that there be a decision procedure for deciding whether a formula is well-formed.
- A set of axioms, or axiom schemata, consisting of well-formed formulas.
- A set of inference rules. A well-formed formula that can be inferred from the axioms is known as a theorem of the formal system.

Three theorems of lambda calculus are
- beta-conversion
- alpha-conversion
- eta-conversion
Lambda-reduction (also called lambda conversion) refers to all three

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
- http://www.mathematicaguidebooks.org/
- Seldin (2006). The logic of Curry and Church. In Handbook of the History of Logic, vol.5: Logic from Russell to Church, p. 819—874. North-Holland: Amsterdam. 
  - https://www.elsevier.com/search-results?labels=books&book-series=Handbook%20of%20the%20History%20of%20Logic&sort=document.published-asc
  - https://www.sciencedirect.com/handbook/handbook-of-the-history-of-logic
  - https://people.uleth.ca/~jonathan.seldin/CCL.pdf
- http://cs.brown.edu/people/jsavage/book/
  - https://cs.brown.edu/people/jsavage/book/pdfs/ModelsOfComputation.pdf
  - Types: [Church (1940). A formulation of the simple theory of types. Journal of Symbolic Logic 5(2):56—68](www.classes.cs.uchicago.edu/archive/2007/spring/32001-1/papers/church-1940.pdf).
