Comments:

```nix
# This is a comment
```

We have null (there's not scape from it mfer!):

```nix
null
```

Boolean values:

```nix
true
false
```

Numbers. Integers and Floats:

```nix
0
0.22
```

Strings:

```nix
"a boring single line string"

''a nice
multi line
string''
```

Lists are space-separated element inside ```[``` and ```]``` which types are
heterogeneous:

```nix
[ true 0 0.22 "string" ]
```

Atribute set. Or also called object, dictionary or record in other places.
The syntax is multiple ```"key name"``` = ```value``` elements separated by
```;```. Never forget the ending ```;``` or it won't "compile":

```nix
{ A = 0; B = "string"; }
# Attribute sets can be nested:
{ A = { B = 0; }; }
# The line above is the same as:
{ A.B = 0; }
```

Recursive attribute set allows the attributes we are defining to be re-used in
the scope of the attribute set. So we can refer recursively other elements of
the set when defining attributes:

```nix
rec { A = 0; B = A; }
# Results in the same record as:
{ A = 0; B = 0; }
```

Local variable definition. The variable defined after ```let``` are avaibale in
the scope after ```in```:

```nix
myAtrSet = let
        numberA = 0;
        numberB = 1;
in { A = numberA; B = numberB; }
```

Shortcut for ```A = B.A```:

```nix
inherit (B) A;
```

Builtin functions:

```nix
builtins.getEnv "PATH"
```

Function definition and calling:

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
```

