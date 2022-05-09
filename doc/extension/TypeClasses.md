# Type Classes

> 4.3.1 Class Declarations
> ```
> topdecl     → class [scontext =>] tycls tyvar [where cdecls]
> scontext    → simpleclass
>             | ( simpleclass1 , … , simpleclassn )             (n ≥ 0)
> simpleclass → qtycls tyvar
> cdecls      → { cdecl1 ; … ; cdecln }                         (n ≥ 0)
> cdecl       → gendecl
>             | (funlhs | var) rhs
> ```
> A ```class``` declaration introduces a new class and the operations (class
> methods) on it. A class declaration has the general form:
>
> ```class cx => C u where cdecls```
>
> This introduces a new class name ```C```; the type variable ```u``` is scoped
> only over the class method signatures in the class body. The context ```cx```
> specifies the superclasses of ```C```, as described below; the only type
> variable that may be referred to in ```cx``` is ```u```.
>
> The superclass relation must not be cyclic; i.e. it must form a directed
> acyclic graph.
>
> The ```cdecls``` part of a ```class``` declaration contains three kinds of
> declarations:
>
> 1. The class declaration introduces new class methods ```vi```, whose scope
>    extends outside the ```class``` declaration. The class methods of a class
>    declaration are precisely the ```vi``` for which there is an explicit type
>    signature ```vi :: cxi => ti``` in ```cdecls```. Class methods share the
>    top level namespace with variable bindings and field names; they must not
>    conflict with other top level bindings in scope. That is, a class method
>    can not have the same name as a top level definition, a field name, or
>    another class method.
>    The type of the top-level class method ```vi``` is:
>    ```vi :: ∀u,w. (Cu,cxi) ⇒ ti```. The ```ti``` must mention ```u```; it may
>    mention type variables ```w``` other than ```u```, in which case the type
>    of ```vi``` is polymorphic in both ```u``` and ```w```. The ```cxi``` may
>    constrain only ```w```; in particular, the ```cxi``` may not constrain 
>    ```u```. For example:
>    ```haskell
>    class Foo a where
>            op :: Num b => a -> b -> a
>    ```
>    Here the type of ```op``` is
>    ```∀ a, b. (Foo a, Num b)   ⇒  a  →  b  →  a```.
> 2. The ```cdecls``` may also contain a ```fixity``` declaration for any of 
>    the class methods (but for no other values). However, since class methods
>    declare top-level values, the fixity declaration for a class method may
     alternatively appear at top level, outside the class declaration.
> 3. Lastly, the ```cdecls``` may contain a default class method for any of
>    the ```vi```. The default class method for ```vi``` is used if no binding
>    for it is given in a particular ```instance``` declaration (see Section
>    4.3.2). The default method declaration is a normal value definition,
>    except that the left hand side may only be a variable or function 
>    definition. For example:
>    ```haskell
>    class Foo a where
>            op1, op2 :: a -> a
>            (op1, op2) = ... 
>    ```
>    is not permitted, because the left hand side of the default declaration
>    is a pattern.
>
> Other than these cases, no other declarations are permitted in ```cdecls```.
> 
> A ```class``` declaration with no ```where``` part may be useful for
> combining a collection of classes into a larger one that inherits all of the
> class methods in the original ones. For example:
>
> ```class  (Read a, Show a) => Textual a```
>
> In such a case, if a type is an instance of all superclasses, it is not
> automatically an instance of the subclass, even though the subclass has no
> immediate class methods. The ```instance``` declaration must be given 
> explicitly with no where part.
>
> [Haskell 2010 Report - 4.3.1 Class Declarations](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)


> In a predicate such as ```Eq a```, we refer to ```Eq``` as the class name, and
> to ```a``` as the class parameter. Were it not for the use of a restricted
> character set, constraints like this might instead have been written in the
> form ```a ∈ Eq```, reflecting an intuition that ***```Eq``` represents a set
> of types of which ```a``` is expected to be a member***. The Haskell syntax,
> however, which looks more like a curried function application, suggests that
> it might be possible to allow classes to have more than one parameter. For
> example, what might a predicate of the form ```R a b``` mean, where two
> parameters ```a``` and ```b``` have been provided? The obvious answer is to
> interpret ```R``` as a two-place relation between types, and to read
> ```R a b``` as the assertion that ```a``` and ```b``` are related by ```R```.
> This is a natural generalization of the one parameter case because sets are
> just one-place relations. More generally, we can interpret an n parameter
> class by an n-place relation on types.
>
> [Type Classes with Functional Dependencies, Mark P. Jones, In Proceedings of the 9th European Symposium on Programming, ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html).

```haskell
instance Eq Bool where
        x == y = if x then y else not y

instance (Eq a, Eq b) ⇒ Eq (a, b) where
        (x , y) == (u, v ) = (x == u && y == v )
```

> In this particular context, the ⇒ symbol should not be read as implication; in
> fact reverse implication would be a more accurate reading, the intention being
> that every instance of ```Ord``` is also an instance of ```Eq```. Thus
> ```Eq``` plays the role of a superclass of ```Ord```.
>
> [Type Classes with Functional Dependencies, Mark P. Jones, In Proceedings of the 9th European Symposium on Programming, ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html).

- [Functional Programming with Overloading and Higher-Order Polymorphism, by Mark P Jones, in Advanced School of Functional Programming, 1995](http://web.cecs.pdx.edu/~mpj/pubs/springschool.html).
- > In most implementations, the presence of a predicate in a function’s type
indicates that an implicit parameter will be added to pass some appropriate
evidence for that predicate at run-time. For example, we might use an
implementation of equality on values of type ```a``` as evidence for a predicate
of the form ```Eq a```. Details of this implementation scheme may be provided by
Wadler and Blott in the paper where type classes were first described:
  - P. Wadler and S. Blott. How to make ad hoc polymorphism less ad hoc. In Proceedings of 16th ACM Symposium on Principles of Programming Languages, pages 60–76, Jan 1989.
- In the decade since that paper was published, many other applications for type
  classes have been discovered.
  - S. Peyton Jones, M. Jones, and E. Meijer. Type classes: Exploring the design
space. In Proceedings of the Second Haskell Workshop, Amsterdam, June 1997.
