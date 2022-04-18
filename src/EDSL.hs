{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

main :: IO ()
main = putStrLn "EDSL!"

-- The "problem":
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

data Free f a = Pure a | Free (f (Free f a))
        --deriving Functor

instance Functor f => Functor (Free f) where
        fmap f = go where
                go (Pure a)  = Pure (f a)
                go (Free fa) = Free (go <$> fa)

instance Functor f => Applicative (Free f) where
        pure = Pure
        Pure a <*> Pure b = Pure $ a b
        Pure a <*> Free mb = Free $ fmap a <$> mb
        Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
        return = pure
        Pure a >>= f = f a
        Free m >>= f = Free ((>>= f) <$> m)

{--
liftF :: Functor f => f a -> Free f a
liftF f = Free (\a -> Free f a)
--}

data PLSentenceV2 t a =
          PLNotV2 t (t -> a)
        | PLAndV2 t t (t -> a)
        | PLOrV2  t t (t -> a)
        | Input (t -> a)
        | Output t a
        deriving Functor

type FreeAST t = Free (PLSentenceV2 t)

{--
input :: FreeAST t t
input = liftF $ Input id

plAnd :: t -> t -> FreeAST t t
plAnd x y = liftF $ PLAndV2 x y id

output :: t -> FreeAST t ()
output x = liftF $ Output x ()
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

type PLSentence a = AST PLSentenceV3 (Full a)

{--
The purpose of abstraction is not to be vague, but to create a new semantic
level in which one can be absolutely precise.
- Edsger Dijkstra
--}
