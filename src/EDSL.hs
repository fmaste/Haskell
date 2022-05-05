{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

main :: IO ()
main = putStrLn "EDSL!"

-- Version 0: The "problem":
--------------------------------------------------------------------------------

-- A deep EDSL (Embedded Domain Specific Language)
-- A syntax tree for a propositional logic language.

data PLSentenceV0 =
          PLVarV0 Int -- "var" or "atomic" is frequently used.
        | PLNotV0 PLSentenceV0
        | PLAndV0 PLSentenceV0 PLSentenceV0
        | PLOrV0 PLSentenceV0 PLSentenceV0

-- Version: Decorated syntax tree: How to enhance the final version:
--------------------------------------------------------------------------------

-- Decorated syntax trees / GHC's trees that grow:
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/hs-syn-type
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/handling-source-locations
-- https://www.microsoft.com/en-us/research/publication/trees-that-grow/

-- Each data type has an extension constructor.
-- Each data construcutor has an extension field.
-- Selected fields, often the recursive ones, have an extension wrapper.

data PLSentenceGHC a =
          PLVarGHC (XVar a) Int
        | PLNotGHC (XNot a)
                (XRec a (PLSentenceGHC a))
        | PLAndGHC (XAnd a)
                (XRec a (PLSentenceGHC a)) (XRec a (PLSentenceGHC a))
        | PLOrGHC  (XOr a)
                (XRec a (PLSentenceGHC a)) (XRec a (PLSentenceGHC a))
        | PLExt (XPLExt a)

type family XVar x
type family XNot x
type family XAnd x
type family XOr x
type family XPLExt x

type family XRec x a

type instance XVar () = ()
type instance XNot () = ()
type instance XAnd () = ()
type instance XOr () = ()
type instance XPLExt () = ()

type instance XRec () a = ()

-- Version: One size fits all lambda calculus:
--------------------------------------------------------------------------------

-- Fundamental to all functional languages is the most atomic notion of
-- composition, function abstraction of a single variable. The lambda calculus
-- consists very simply of three terms and all valid recursive combinations
-- thereof:

type Name = Int

data LambdaAST =
          Variable Name
        | Literal Literal
        | Application LambdaAST LambdaAST
        -- | A lambda term is said to bind its variable. Like f(x) = e in math.
        | Lambda Name LambdaAST

data Literal = LInt Int | LString String

-- IMO Building something as abstract as `glamda` makes EDSL meaningless.
-- https://github.com/goldfirere/glambda

-- IMO same thing if we need to add a type system like page 56 of:
-- http://dev.stephendiehl.com/fun/WYAH.pdf
-- https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter5/calc_typed

-- Building Haskell's already in place functionalities from scratch!

-- Version: Common GADT problem:
--------------------------------------------------------------------------------

-- Imagine a simple embedded domain specific language (EDSL) for simple
-- arithmetical expressions:

data Expr =
          ExprI Int
        | ExprB Bool
        | ExprAdd Expr Expr
        | ExprMul Expr Expr
        | ExprEq  Expr Expr

-- This is valid and we are not trying to build a PHP abstract syntax tree:
exprExample :: Expr
exprExample = (ExprEq (ExprI 1) (ExprB True) )

{--
We can add a type variable a and export an "API" / smart constructors to the end
-- user with functions like this one:

i :: Int  -> Expr Int
...

b :: Bool -> Expr Bool
...

exprAdd :: Expr Int -> Expr Int -> Expr Int
...

But a function eval

eval :: Expr a -> a
...

Won't type check!
--}

-- This is solved with a phantom type. A.K.A GADT.

data PhatomExpr a where
        PhatomExprI :: Int -> PhatomExpr Int
        PhatomExprB :: Bool -> PhatomExpr Bool
        PhatomExprAdd :: PhatomExpr Int -> PhatomExpr Int -> PhatomExpr Int
        PhatomExprMul :: PhatomExpr Int -> PhatomExpr Int -> PhatomExpr Int
        PhatomExprEq  :: Eq a => PhatomExpr a -> PhatomExpr a -> PhatomExpr Bool

-- Danger Overbuilding!!!
-- Like above we somehow "solved" the typing problem but our initial problem
-- only deals with Bool expressions.

-- Version 1: Mix of lambda calculus and GADTs.
--------------------------------------------------------------------------------

-- One way to separate the tree structure from the symbols is to make symbol
-- application explicit:

data PLSentenceV1 a where
        PLVarV1 :: Int -> PLSentenceV1 Bool
        PLNotV1 :: PLSentenceV1 ( Bool -> Bool           )
        PLAndV1 :: PLSentenceV1 ( Bool -> (Bool -> Bool) )
        PLOrV1 ::  PLSentenceV1 ( Bool -> (Bool -> Bool) )
        PLAppV1 :: PLSentenceV1 (a -> b) -> PLSentenceV1 a -> PLSentenceV1 b

(<->>) :: PLSentenceV1 (a -> b) -> PLSentenceV1 a -> PLSentenceV1 b
(<->>) = PLAppV1

-- Here, PLNotV1, PLAndV1 and PLOrV1 are function-valued symbols (i.e. symbols
-- whose semantic value is a function), and the only thing we can do with those
-- symbols is to apply them to arguments using App.

-- What we have gained with this rewriting is the ability to traverse the tree
-- without necessarily mentioning any symbols.

plSizeV1 :: PLSentenceV1 a -> Int
plSizeV1 (PLAppV1 f a) = (plSizeV1 f) + (plSizeV1 a)
plSizeV1 (PLVarV1 _) = 0
plSizeV1 _ = 1

evaluateV1 :: (Int -> Bool) -> PLSentenceV1 a -> Bool
evaluateV1 vf (PLVarV1 i) = vf i
evaluateV1 vf (PLAppV1 PLNotV1 b) = not $ evaluateV1 vf b
evaluateV1 vf (PLAppV1 (PLAppV1 PLAndV1 a) b) =
        (evaluateV1 vf a) && (evaluateV1 vf b)
evaluateV1 vf (PLAppV1 (PLAppV1 PLOrV1 a ) b) =
        (evaluateV1 vf a) ||  (evaluateV1 vf b)
-- Still some invalid tree structures can be built?
-- Yes: (PLAppV1 PLAndV1 (PLVarV1 1)) type checks.
evaluateV1 _ _ = error ""

exampleV1 :: PLSentenceV1 Bool
exampleV1 = PLAndV1 <->> (PLVarV1 1) <->> (PLVarV1 0)
-- exampleV1 = PLAppV1 (PLAppV1 PLAndV1 (PLVarV1 1)) (PLVarV1 0)

{--
*Main> plSizeV1 exampleV1 
1
*Main> evaluateV1 (> 0) exampleV1 
False
*Main>
--}

-- However, even though we have achieved a certain kind of generic programming,
-- it is limited to a single type, which makes it quite uninteresting. Luckily,
-- the idea can be generalized.

-- Version 2:
--------------------------------------------------------------------------------

-- TODO: "Free Monads" and fix point recursion:
-- https://serokell.io/blog/introduction-to-free-monads
-- https://hackage.haskell.org/package/free
-- https://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

-- Given a functor f, Free f is the monad it generates:
data Free f a =
          Pure a -- Like `Applicative`'s `pure` and `Monad`'s `return`
        | Free (f (Free f a)) -- Like monadic `join`

data PLSentenceF t =
          PLNotF t
        | PLAndF t t
        | PLOrF  t t
        deriving Functor

type PLSentence a = Free PLSentenceF a

-- > :t Free (PLAndF (Pure 1) (Pure 0))
-- Free (PLAndF (Pure 1) (Pure 0)) :: Num a => Free PLSentenceF a
-- > :t Free ( PLAndF (Pure 1) (Free (PLOrF (Pure 1) (Pure 0))) )
-- Free ( PLAndF (Pure 1) (Free (PLOrF (Pure 1) (Pure 0))) )
-- :: Num a => Free PLSentenceF a

evaluate :: (a -> Bool) -> (PLSentence a) -> Bool
evaluate vf (Pure v) = vf v
evaluate vf (Free (PLNotF a  )) = not $ evaluate vf a
evaluate vf (Free (PLAndF a b)) = (evaluate vf a) && (evaluate vf b)
evaluate vf (Free (PLOrF  a b)) = (evaluate vf a) || (evaluate vf b)

printPl :: Show a => (PLSentence a) -> String
printPl (Pure v) = show v
printPl (Free (PLNotF a  )) = "NOT ( " ++ (printPl a) ++ " ) "
printPl (Free (PLAndF a b)) = "(" ++ (printPl a) ++ ") AND (" ++ (printPl b) ++ ")"
printPl (Free (PLOrF  a b)) = "(" ++ (printPl a) ++ ") OR  (" ++ (printPl b) ++ ")"

plVar :: a -> (PLSentence a)
plVar v = Pure v

plNot :: (PLSentence a) -> (PLSentence a)
plNot a = Free (PLNotF a)

plAnd :: (PLSentence a) -> (PLSentence a) -> (PLSentence a)
plAnd a b = Free (PLAndF a b)

plOr :: (PLSentence a) -> (PLSentence a) -> (PLSentence a)
plOr a b = Free (PLOrF a b)

example1 :: PLSentence Int
example1 = plAnd (plVar 1) (plOr (plVar 0) (plVar 1))

-- > evaluate1 (>0) example1
-- True

-- > printPl example1
-- "(1) AND ((0) OR  (1))"

{-- TODO: Monadic version of this??? Is not sequential!

instance Functor f => Functor (Free f) where
        fmap g (Pure a) = Pure (g a)
        fmap g (Free f) = Free (fmap (fmap g) f) -- Like g o f in math notation.

instance Functor f => Applicative (Free f) where
        pure = Pure
        Pure g <*> Pure a = Pure $ g a
        Pure g <*> Free mb = Free $ fmap g <$> mb
        Free mg <*> b = Free $ (<*> b) <$> mg

-- Given that f is a Functor, we get that Free is a Monad for free:
instance Functor f => Monad (Free f) where
        return = pure
        Pure a >>= f = f a
        Free m >>= f = Free (fmap (>>= f) m)

{--
liftF :: Functor f => f a -> Free f a
liftF f = Free (\a -> Free f a)
--}
liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

plNotM :: a -> (PLSentence a)
plNotM x = Free (PLNotF (Pure x))

plAndM :: a -> a -> (PLSentence a)
plAndM x y = Free (PLAndF (Pure x) (Pure y))

plOrM :: a -> a -> (PLSentence a)
plOrM x y = Free (PLOrF (Pure x) (Pure y))

example2 :: PLSentence Int
example2 = do
        var0 <- plVar 0
        var1 <- plVar 1
        pl <- plAndM var0 var1
        plOrM var0 pl
        --return pl

-- > printPl example2
-- "((0) OR  (0)) AND ((0) OR  (1))"

--}

-- Version 3: Solving the expression problem:
--------------------------------------------------------------------------------

-- From "A Generic Abstract Syntax Model for Embedded Languages" and
-- "Data types a la carte", or the "syntactic" hackage package!

-- If we lift out the three symbols from PLSentenceV1 and replace them with a
-- single symbol constructor, we reach the following syntax tree model:

data AST dom sig where
        -- The symbol/variable.
        Sym :: dom sig -> AST dom sig -- Lifted "type Sym = String" ?
        -- The application.
        (:$) :: (a :-> sig) -> AST dom (Full sig) -> AST dom sig

-- The AST type is parameterized on the symbol domain dom, and the Sym
-- constructor introduces a symbol from this domain. The type (:->) is isomorphic
-- to the function arrow, and Full a is isomorphic to a.

infixl 1 :$

newtype Full a = Full {result :: a}

newtype a :-> b = Partial (a -> b)

infixr :->

-- As will be seen later, these types are needed to be able to distinguish
-- function-valued expressions from partially applied syntax trees.

data PLSentenceV3 a where
        PLVarV3 :: Int -> PLSentenceV3 (Full Bool)
        PLNotV3 :: PLSentenceV3 (Bool :-> Full Bool)
        PLAndV3 :: PLSentenceV3 (Bool :-> Bool :-> Full Bool)
        PLOrV3 :: PLSentenceV3 (Bool :-> Bool :-> Full Bool)

type PLSentenceAST a = AST PLSentenceV3 (Full a)

{--
The purpose of abstraction is not to be vague, but to create a new semantic
level in which one can be absolutely precise.
- Edsger Dijkstra
--}

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f i [] = i
myFoldl f i (x:xs) = myFoldl f (f i x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f i [] = i
myFoldr f i (x:xs) = f x (myFoldr f i xs)
