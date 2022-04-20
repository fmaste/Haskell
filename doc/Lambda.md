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

- http://www.mathematicaguidebooks.org/
- http://cs.brown.edu/people/jsavage/book/
  - https://cs.brown.edu/people/jsavage/book/pdfs/ModelsOfComputation.pdf
