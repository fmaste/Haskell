# Lambda calculus

The subarea of [Theoretical Computer Science](https://en.wikipedia.org/wiki/Theoretical_computer_science) called [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (also written as λ-calculus) is a formal system for expressing computations developed by [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) and [Stephen Kleene](https://en.wikipedia.org/wiki/Stephen_Cole_Kleene). Probably the highest level theoretical/formal model of computation that will ever be conceived.

Like any [formal system](https://en.wikipedia.org/wiki/Formal_system), it's an abstract structure used for inferring theorems from axioms according to a set of rules.

It is based on function abstraction and application using variable binding and substitution.

It's a set of [Axioms](https://en.wikipedia.org/wiki/Axiom) (something taken to be true as a starting point for further reasoning) 
- A finite set of symbols, known as the alphabet, which concatenate formulas, so that a formula is just a finite string of symbols taken from the alphabet.
- A grammar consisting of rules to form formulas from simpler formulas. A formula is said to be well-formed if it can be formed using the rules of the formal grammar. It is often required that there be a decision procedure for deciding whether a formula is well-formed.
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
- [History of the Church–Turing thesis](https://en.wikipedia.org/wiki/History_of_the_Church%E2%80%93Turing_thesis)
- [General recursive functions / partial recursive function / μ-recursive functions](https://en.wikipedia.org/wiki/General_recursive_function)
  - The smallest class of functions (with arbitrarily many arguments) that is closed under composition, recursion, and minimization, and includes zero, successor, and all projections.
- The equivalence between the partial recursive functions and the Turing computable functions was shown by Kleene:
  - S. C. Kleene, “General Recursive Functions of Natural Numbers,” Math. Annalen 112 (1936), 727–742.
  - It seems to be incidental that the calculus invented by Church turned out to be Turing complete, although Church later used the lambda calculus for what he called the Effectively computable functions (1936).
- http://www.mathematicaguidebooks.org/
- Seldin (2006). The logic of Curry and Church. In Handbook of the History of Logic, vol.5: Logic from Russell to Church, p. 819—874. North-Holland: Amsterdam. 
  - https://www.elsevier.com/search-results?labels=books&book-series=Handbook%20of%20the%20History%20of%20Logic&sort=document.published-asc
  - https://www.sciencedirect.com/handbook/handbook-of-the-history-of-logic
  - https://people.uleth.ca/~jonathan.seldin/CCL.pdf
- http://cs.brown.edu/people/jsavage/book/
  - https://cs.brown.edu/people/jsavage/book/pdfs/ModelsOfComputation.pdf
