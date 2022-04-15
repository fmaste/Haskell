# [Formal Language](https://en.wikipedia.org/wiki/Formal_language)

In logic, mathematics, computer science, and linguistics, a formal language consists of ***words whose letters are taken from an alphabet*** and are ***well-formed according to a specific set of rules***.

In formal languages words (as in string like ```if a > 0 then True else False```) is a finite sequence of [symbols, letters, or tokens](https://en.wikipedia.org/wiki/Symbol_(formal)) (as in ```if```, ```then```, ```else```, ```>```, ```a```) that are chosen from a set called an [alphabet](https://en.wikipedia.org/wiki/Alphabet_(computer_science)).

The words that belong to a particular formal language, that adhere to the rules specified on the formal language specification, are called well-formed words or ***well-formed formulas***.

There are many languages that can be described by rules, such as [regular languages](https://en.wikipedia.org/wiki/Regular_language) or [context-free languages](https://en.wikipedia.org/wiki/Context-free_language). This two provide a good compromise between expressiveness and ease of parsing, and are widely used in practical applications.

#### TODO: Difference between regular and context free and the rest

## Abstract Syntax Tree

Programming languages compilers usually have a two stage syntactic [parser](https://en.wikipedia.org/wiki/Parser):
- [Lexical analysis](https://en.wikipedia.org/wiki/Lexical_analysis): the input character stream is split into meaningful symbols defined by a grammar of [regular expressions](https://en.wikipedia.org/wiki/Regular_expression).
- [Syntactic](https://en.wikipedia.org/wiki/Syntax_(logic)) analysis: which is checking that the tokens form an allowable expression.

The final phase is [semantic](https://en.wikipedia.org/wiki/Semantics_(computer_science)) [parsing](https://en.wikipedia.org/wiki/Semantic_analysis_(compilers)) or [analysis](https://en.wikipedia.org/wiki/Formal_semantics_(logic)). This usually done with an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) that is the output of the two stage parser described above.

This structure is used by subsequent stages of the compiler to in the end, for example, generate an executable or intermediate code required by a virtual machine to execute.

## Grammar

A formal grammar determines which symbols are well-formed formulas in a formal language. Describes how to do the syntactic step, step 2 detailed above, to form strings from a language's alphabet that are valid according to the language's syntax.

A grammar does not describe the meaning of the strings or what can be done with them. A formal grammar is defined as a set of production rules for such strings in a formal language.

 In computer science a formal language is often defined by means of a [formal grammar](https://en.wikipedia.org/wiki/Formal_grammar) such as a [regular grammar](https://en.wikipedia.org/wiki/Regular_grammar) or [context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar), which consists of its formation rules.

### TODO: Regular grammar is a subset of context free grammar, but not every context free grammar is a regular grammar.

# EDSL

Haskell's mathematical notation and rich and versatile type system makes it a good playground for domain specific languages or EDSL for short.

For example a structure like the one below allows us to build a formal language specification with grammar, parser and abstract syntax tree all at once.

```
data Expr a =
        Var a |
        Not (Expr a) |
        And (Expr a) (Expr a) |
        Or (Expr a) (Expr a)
```

You can rely on the type system so only syntactically and semantically valid expressions can be formed and concentrate on the abstract syntax tree to produce an evaluation or transformation function.

```
eval :: (a -> Bool) -> Expr a -> Bool
eval f (Var i) = f i
eval f (Not a) = if (eval f a) then False else True
eval f (And a b) = if (eval f a) then (eval f b) else False
eval f (Or a b) = if (eval f a) then True else (eval f b)

>> eval (>= 1) ( Or (Not $ Var 1 ) ( And (Var 2) (Var 3) ) )
True
```

The tricky part is using the type system to create an EDSL that is easy to use by the end user and that its code is more maintainable and reusable by the developer.

# The “expression problem”

In 1998, Philip Wadler coined the “expression problem”[^1]: `“The Expression Problem is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts).”`

[^1]: http://www.daimi.au.dk/~madst/tool/papers/expression.txt

The problem with types such as ```Expr``` above is that:
- They are not extensible. It is perfectly possible to add new interpretation functions in the same way as ```eval```, but unfortunately, adding new constructors is not that easy. If we want to add a new constructor, say for XOR, not only do we need to edit and recompile the definition of ```Expr```, but also all existing interpretation functions.
- Another problem with ```Expr``` is the way that the recursive structure of the tree has been mixed up with the symbols in it: It is not possible to traverse the tree without pattern matching on the constructors, and this prevents the definition of generic traversals where only the “interesting” constructors have to be dealt with.
- The output value is always ```Bool```

We are going to look at different solutions to this problem problem.

## Generalised Algebraic Data Types (GADTs)

See [Phantom Types](doc/Phantom.md) for the most widely used extensions to data types that is used to tackle some of this problems.

## A Generic Abstract Syntax Model for Embedded Languages

- https://emilaxelsson.github.io/documents/axelsson2012generic.pdf
- https://emilaxelsson.github.io/documents/axelsson2012generic-slides.pdf
- http://hackage.haskell.org/package/syntactic-1.0

# Further reading

https://www.cambridge.org/core/services/aop-cambridge-core/content/view/14416CB20C4637164EA9F77097909409/S0956796808006758a.pdf/data-types-a-la-carte.pdf
https://www.reddit.com/r/haskell/comments/nojtd2/annotate_ast_with_location_information/
