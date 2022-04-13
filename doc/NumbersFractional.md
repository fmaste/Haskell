
```
class Num a => Fractional a where
        (/) :: a -> a -> a 
        recip :: a -> a 
        fromRational :: Rational -> a 
```

```
class Fractional a => Floating a where
        pi :: a 
        exp :: a -> a 
        log :: a -> a 
        sqrt :: a -> a 
        (**) :: a -> a -> a
        logBase :: a -> a -> a 
        sin :: a -> a 
        cos :: a -> a 
        tan :: a -> a 
        asin :: a -> a 
        acos :: a -> a 
        atan :: a -> a 
        sinh :: a -> a 
        cosh :: a -> a 
        tanh :: a -> a 
        asinh :: a -> a 
        acosh :: a -> a 
        atanh :: a -> a 
```

```
class (Real a, Fractional a) => RealFrac a where
        properFraction :: Integral b => a -> (b, a) 
        truncate :: Integral b => a -> b 
        round :: Integral b => a -> b 
        ceiling :: Integral b => a -> b 
        floor :: Integral b => a -> b 
```

```
class (RealFrac a, Floating a) => RealFloat a where
        properFraction :: Integral b => a -> (b, a) 
        truncate :: Integral b => a -> b 
        round :: Integral b => a -> b 
        ceiling :: Integral b => a -> b 
        floor :: Integral b => a -> b 
```

```
class (RealFrac a, Floating a) => RealFloat a where
        floatRadix :: a -> Integer
        floatDigits :: a -> Int
        floatRange :: a -> (Int, Int) 
        decodeFloat :: a -> (Integer, Int) 
        encodeFloat :: Integer -> Int -> a 
        exponent :: a -> Int
        significand :: a -> a 
        scaleFloat :: Int -> a -> a 
        isNaN :: a -> Bool
        isInfinite :: a -> Bool
        isDenormalized :: a -> Bool
        isNegativeZero :: a -> Bool
        isIEEE :: a -> Bool
        atan2 :: a -> a -> a 
```
