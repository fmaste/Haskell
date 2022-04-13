# Functions on strings

```
lines :: String -> [String]
words :: String -> [String]
unlines :: [String] -> String
unwords :: [String] -> String
```

# Converting to and from String

## Converting to String

```
type ShowS = String -> String
```

```
class Show a where
        showsPrec :: Int -> a -> ShowS
        show :: a -> String
        showList :: [a] -> ShowS
```

```
shows :: Show a => a -> ShowS
showChar :: Char -> ShowS
showString :: String -> ShowS
showParen :: Bool -> ShowS -> ShowS
```

## Converting from String

```
type ReadS a = String -> [(a, String)]
```

```
class Read a where
        readsPrec :: Int -> ReadS a
        readList :: ReadS [a]
```

```
reads :: Read a => ReadS a
readParen :: Bool -> ReadS a -> ReadS a
read :: Read a => String -> a
lex :: ReadS String
```

