TODO: Formal grammars and algebras

# Algebras

Data types a la carte

https://www.reddit.com/r/haskell/comments/nojtd2/annotate_ast_with_location_information/

https://gist.github.com/blankhart/ea84d43489d4799a4fe98cf232d4d6eb

# Recursion schemes:

https://hackage.haskell.org/package/recursion-schemes
http://newartisans.com/2018/04/win-for-recursion-schemes/

https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/
https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

http://newartisans.com/2018/04/win-for-recursion-schemes/

You have a recursive data structure:
```
data List a = Nil | Cons a (List a)

data Rose a = Leaf a | Branch [Rose a]

data Expr = Var Id | Abs Id Expr | App Expr Expr
```

You replace recursive calls with a new type parameter: In each case, you get a Functor.
```
data ListF a r = NilF | ConsF a r deriving Functor

data RoseF a r = LeafF a | BranchF [r] deriving Functor

data ExprF = Var Id | Abs Id r | App r r deriving Functor
```

You think "... smaller is good, but the extra parameter is bad ...".
```
```

# TODO:

https://bartoszmilewski.com/2016/12/27/monads-categorically/
