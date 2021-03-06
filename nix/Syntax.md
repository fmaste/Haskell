# Nix language syntax

The [Nix language](https://nixos.org/manual/nix/stable/expressions/expression-language.html)
is used write expressions that produce derivations.

> The Nix expression language is a ***pure, lazy, functional language***. Purity
> means that ***operations in the language don't have side-effects*** (for
> instance, there is no variable assignment). Laziness means that arguments to
> functions are ***evaluated only when they are needed***. Functional means that
> ***functions are “normal” values that can be passed around*** and manipulated
> in interesting ways. The language is not a full-featured, general purpose
> language. ***Its main job is to describe packages, compositions of packages,
> and the variability within packages***.
>
> [Nix Expression Language](https://nixos.org/manual/nix/stable/expressions/expression-language.html)

Inherited from its functional programming roots, in Nix, everything is an
expression, there are no statements, and values are immutable.

Even scope analysis in Nix is lazy. This undefined `d` does not complain at all:
```nixs
let
        a = { a = 0; };
        b = { b = 1; };
        c = with a; with b; [ a b c d ];
in pkgs.stdenv.mkDerivation {
        name = "hello";
        buildCommand = "echo 'Hello world!' > $out";

}
```

A ```.nix``` contains one and only one of the values or language constructs that
are explained below.

## Comments:

```nix
# This is a comment

/* THIS
IS A COMMENT!
*/
```

## Simple values

Nix has integer, floating point, string, path, boolean and null
[simple](https://nixos.org/manual/nix/stable/expressions/language-values.html)
types.

### Numbers:

Integers and floats:

```nix
0
0.22
.22e10
```

Pure integer operations will always return integers, whereas any operation
involving at least one floating point number.

### Strings:

```nix
"a boring single line string"

''a nice
indented
string''
```

The special characters ```"``` and ```\``` and the character sequence ```${```
must be escaped by prefixing them with a backslash (```\```). Newlines, carriage
returns and tabs can be written as ```\n```, ```\r``` and ```\t```,
respectively.

#### Antiquotation

You can include the result of an expression into a string by enclosing it in
```${...}```, a feature known as antiquotation.

Instead of writing this
```"--with-freetype2-library=" + freetype + "/lib"```
you do this on nix
```"--with-freetype2-library=${freetype}/lib"```.

### Paths

A path must contain at least one slash to be recognized as such.

```console
nix-repl> 4/2
/home/nix/4/2
nix-repl> ~/foo
/home/fmaste/foo
nix-repl> <nixpkgs>
/home/fmaste/.nix-defexpr/channels/nixpkgs
```

Square braces as in ```<nixpkgs>``` resolve from ```NIX_PATH``` environment
variable (that's what the docs say, I have no ```NIX_PATH```).

Nix is not a general purpose language, it's a domain-specific language for
writing packages. That's why ```4/2``` is a path, not a division.

### Boolean values

```nix
true
false
```

### We have null

There's no escape from it mfer:

```nix
null
```

## Lists

Are whitespace-separated elements between ```[``` and ```]``` which types are
heterogeneous:

```nix
[ true 0 0.22 "string" ]
```

Note that lists are only lazy in values, and they are strict in length.

## Attribute sets

Or also called object, dictionary or record in other places. The syntax is
multiple ```"key name"``` = ```value``` elements separated by ```;```. Never
forget the ending ```;``` or it won't "compile":

```nix
{ A = 0; B = "string"; }
# Attribute sets can be nested:
{ A = { B = 0; }; }
# The line above is the same as:
{ A.B = 0; }
# Also double-quoted strings:
{ "foo ${bar}" = 1; }
```

In the special case where an attribute name inside of a set declaration
evaluates to null (which is normally an error, as null is not antiquotable),
that attribute is simply not added to the set.

## Language constructs

### Recursive attribute sets

Allows the attributes we are defining to be re-used in the scope of the
attribute set. So we can refer recursively other elements of the set when
defining attributes:

```nix
rec { A = 0; B = A; }
# Results in the same record as:
{ A = 0; B = 0; }
```

### Let

Local variable definition

The variable defined after ```let``` are available in the scope after ```in```:

```nix
myAtrSet = let
        numberA = 0;
        numberB = 1;
in { A = numberA; B = numberB; }
```
* You cannot refer to variables in a let expression outside of it but you can
refer to variables in the let expression when assigning variables, like with
recursive attribute sets.

```console
nix-repl> let a = 0; in let a = 1; in let a = 2; in a
2
```

### Inherit

Or a nice way of saying ```A = A```:

```nix
let
  A = 0;
in
  {
    inherit A;
    B = 1;
  }
```

Or a nice way of saying ```myFunction = myLibrary.myFunction``` that is mostly
used to get a function from another package (an attribute set) into scope:

```nix
inherit (myLibrary) myFunction;
```

 The inherit keyword causes the specified attributes to be bound to whatever
 variables with the same name happen to be in scope.

### Functions

#### Functions definition and calling:

```nix
# a and b are the parameters and the return expression is a + b
add = a: b: a + b
add 1 1
```

Function definition with attribute sets as parameters:

```nix
# Here b has a default value
add = { a, b ? 1 }: a + b
add { a = 1; }
```

Load code:

```nix
import ./more.nix
pkgs = import <nixpkgs> {};
lib = import <nixpkgs/lib>;
```

It's equivalent in other languages is ```eval``` function.

Lambdas / Functions

```nix
double = x: x*2
nix-repl> double
«lambda»
nix-repl> double 3
6

mul = a: (b: a*b)
nix-repl> mul
«lambda»
nix-repl> mul 3
«lambda»
nix-repl> (mul 3) 4
12

nix-repl> mul = s: s.a*s.b
nix-repl> mul { a = 3; b = 4; }
12
nix-repl> mul = { a, b }: a*b
nix-repl> mul { a = 3; b = 4; }
12
nix-repl> mul { a = 3; b = 4; c = 6; }
error: anonymous function at (string):1:2 called with unexpected argument `c', at (string):1:1
nix-repl> mul { a = 3; }
error: anonymous function at (string):1:2 called without required argument `b', at (string):1:1

nix-repl> mul = { a, b ? 2 }: a*b
nix-repl> mul { a = 3; }
6
nix-repl> mul { a = 3; b = 4; }
12

nix-repl> mul = { a, b, ... }: a*b
nix-repl> mul { a = 3; b = 4; c = 2; }

nix-repl> mul = s@{ a, b, ... }: a*b*s.c
nix-repl> mul { a = 3; b = 4; c = 2; }
24
```

## Operators

```nix
nix-repl> 1+2
3
nix-repl> 2-1
1
nix-repl> 2*2
4
```

***Watch out when using division***:

```nix
nix-repl> 4/2
/home/nix/4/2
nix-repl> 4/ 2
2
nix-repl> builtins.div 4 2
2
```

Other operators are ```||```, ```&&``` and ```!``` for booleans, and relational
operators such as ```!=```, ```==```, ```<```, ```>```, ```<=```, ```>=```.

## Built-in functions

The only [built-in constant](https://nixos.org/manual/nix/stable/expressions/builtin-constants.html)
is ```builtins```. This attributes set contains a lot of
[useful functions](https://nixos.org/manual/nix/stable/expressions/builtins.html)
like ```isFloat```, ```typeOf``` to be able to introspect expressions or
```trace``` and ```abort``` to debug.

## Imports

File a.nix:
> 1

File b.nix:
> 2

File add.nix:
> a: b: a+b

```nix
nix-repl> a = import ./a.nix
nix-repl> b = import ./b.nix
nix-repl> add = import ./add.nix
nix-repl> add a b
3
```

nix-repl> builtins.all (a: a > 0) [1 2 3]       
true

nix-repl> builtins.all (a: a > 2) [1 2 3] 
false

## With

```console
nix-repl> let a = 0; with {a = 1;}; with {a = 2;}; a
0
```
