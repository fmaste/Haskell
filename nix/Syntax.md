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
"string"

''a nice
multi line
string''
```

Lists are space separated element inside ```[``` and ```]``` a have heterogeneous
types inside:

```nix
[ true 0 0.22 "string" ]
```

Atribute set. Or also called object, dictionary or record in other places.
The syntax is multiple ```"key name"``` = ```value``` elements separated by
```;```:

```nix
{ A = 0; B = "string"; }
# Can be nested:
{ A = { B = 0; }; }
# And it the same as:
{ A.B = 0; }
```

Recursive attribute set allows the attributes we are defining to be re-used in
the scope of the attribute set. So we can refer recursively other elements of
the set when defining attributes:

```nix
rec { A = 1; B = A; }
```

Local variable definition. The variable defined after ```let``` are avaibale in
the scope after ```in```:

```nix
A = let
        number = 0;
    in { B = number; }
```

Shortcut for ```A = B.A```:

```nix
inherit (B) A;
```

Builtin functions:

```nix
builtins.getEnv "PATH"
```

Function definition:

```nix
# a and b are the parameters and the return expression is a + b
add = a: b: a + b
```


