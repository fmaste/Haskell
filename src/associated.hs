{-# LANGUAGE TypeFamilies #-}

module Main where

main :: IO ()
main = print 'a'

--------------------------------------------------------------------------------

data SuperFastIntMap v = ConsSuperFastIntMap [v]

emptyInt :: SuperFastIntMap v
emptyInt = ConsSuperFastIntMap []

lookupInt :: Int -> SuperFastIntMap v -> Maybe v
lookupInt _ (ConsSuperFastIntMap []) = Nothing
lookupInt _ (ConsSuperFastIntMap (a:_)) = Just a

data SuperEfficientCharMap v = ConsSuperEfficientCharMap [v]

emptyChar :: SuperEfficientCharMap v
emptyChar = ConsSuperEfficientCharMap []

lookupChar :: Char -> SuperEfficientCharMap v -> Maybe v
lookupChar _ (ConsSuperEfficientCharMap []) = Nothing
lookupChar _ (ConsSuperEfficientCharMap (a:_)) = Just a

--------------------------------------------------------------------------------

class MapKey k where
        data family Map k v
        empty :: Map k v
        lookup :: k -> Map k v -> Maybe v

instance MapKey Int where
        data Map Int v = IntMap (SuperFastIntMap v)
        empty = IntMap emptyInt
        lookup k (IntMap intMap) = lookupInt k intMap

instance MapKey Char where
        data Map Char v = CharMap (SuperEfficientCharMap v)
        empty = CharMap $ ConsSuperEfficientCharMap []
        lookup _ (CharMap (ConsSuperEfficientCharMap [])) = Nothing
        lookup _ (CharMap (ConsSuperEfficientCharMap (a:as))) = Just a

-- Can't use it outside. I mean I can buy it's not very useful!
-- Whenever MapKey appears in a type there must be an appropriate context or instances in order to deduce (Map k v)
-- I can even take out the constraint!
outside :: MapKey k => Map k a -> Int
-- This two below are ilegal:
{--
[1 of 1] Compiling Main             ( Workspace/GitHub/fmaste/Haskell/src/associated.hs, interpreted )

Workspace/GitHub/fmaste/Haskell/src/associated.hs:28:10: error:
    • Couldn't match type ‘k’ with ‘Char’
      ‘k’ is a rigid type variable bound by
        the type signature for:
          outside :: forall k a. MapKey k => Map k a -> Int
        at Workspace/GitHub/fmaste/Haskell/src/associated.hs:26:1-37
      Expected type: Map k a
        Actual type: Map Char a
    • In the pattern: CharMap (a : as)
      In an equation for ‘outside’: outside (CharMap (a : as)) = 1
    • Relevant bindings include
        outside :: Map k a -> Int
          (bound at Workspace/GitHub/fmaste/Haskell/src/associated.hs:28:1)
   |
28 | outside (CharMap (a:as)) = 1
   |          ^^^^^^^^^^^^^^
Failed, no modules loaded.
--}
--outside (CharMap (a:as)) = 1
--outside (IntMap (a:as)) = 1
outside _ = 0

-- Also illegal: "'MapKey Char’ has kind ‘Constraint’"
--outside' :: MapKey Char -> Int
--outside' (CharMap (a:as)) = 1

class Nose notused where
        myConst :: notused -> a -> b -> a

class Graph g where
        data Edge g
        data Vertex g
        src :: Edge g -> g -> Vertex g

