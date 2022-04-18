{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

main :: IO ()
main = putStrLn "EDSL!"

-- The "problem":
--------------------------------------------------------------------------------

data PLSentenceV0 =
          PLVarV0 Int
        | PLNotV0 PLSentenceV0
        | PLAndV0 PLSentenceV0 PLSentenceV0
        | PLOrV0 PLSentenceV0 PLSentenceV0


-- Decorated syntax tree version:
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
        | PLNotGHC (XNot a) (PLSentenceGHC a)
        | PLAndGHC (XAnd a) (PLSentenceGHC a) (PLSentenceGHC a)
        | PLOrGHC  (XOr a) (PLSentenceGHC a) (PLSentenceGHC a)
        | PLExt (XPLExt a)

type family XVar x
type family XNot x
type family XAnd x
type family XOr x
type family XPLExt x

type family XRec x a

-- Version 0: Lambda calculus.
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

-- Version 1:
--------------------------------------------------------------------------------

-- One way to separate the tree structure from the symbols is to make symbol
-- application explicit:

data PLSentenceV1 a where
        PLVarV1 :: Int -> PLSentenceV1 a
        PLNotV1 :: PLSentenceV1 (Bool -> Bool)
        PLAndV1 :: PLSentenceV1 (Bool -> Bool -> Bool)
        PLOrV1 :: PLSentenceV1 (Bool -> Bool -> Bool)
        PLAppV1 :: PLSentenceV1 (a -> b) -> PLSentenceV1 a -> PLSentenceV1 b

-- Here, PLNotV1, PLAndV1 and PLOrV1 are function-valued symbols (i.e. symbols
-- whose semantic value is a function), and the only thing we can do with those
-- symbols is to apply them to arguments using App.

-- What we have gained with this rewriting is the ability to traverse the tree
-- without necessarily mentioning any symbols.

plSizeV1 :: PLSentenceV1 a -> Int
plSizeV1 (PLAppV1 f a) = (plSizeV1 f) + (plSizeV1 a)
plSizeV1 _ = 1

exampleV1 :: PLSentenceV1 Bool
exampleV1 = PLAppV1 (PLAppV1 PLAndV1 (PLVarV1 1)) (PLVarV1 2)

{--
evaluateV1 :: PLSentenceV1 Bool -> Bool
evaluateV1   -> Bool
--}

-- However, even though we have achieved a certain kind of generic programming,
-- it is limited to a single type, which makes it quite uninteresting. Luckily,
-- the idea can be generalized.

-- Version 2:
--------------------------------------------------------------------------------

-- https://serokell.io/blog/introduction-to-free-monads

-- Version 3:
--------------------------------------------------------------------------------

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
