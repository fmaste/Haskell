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
