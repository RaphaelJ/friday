{-# LANGUAGE BangPatterns #-}
module Data.RatioInt (RatioInt (numerator, denominator), (%)) where

import qualified Data.Ratio as R

-- | Rational numbers, with numerator and denominator of the 'Int' type.
data RatioInt = RatioInt {
      numerator   :: {-# UNPACK #-} !Int
    , denominator :: {-# UNPACK #-} !Int
    } deriving (Eq, Show, Read)

ratioZeroDenominatorError :: a
ratioZeroDenominatorError = error "Division by zero"

-- | Forms the ratio of two 'Int' numbers.
(%) :: Int -> Int -> RatioInt
x % y =  reduce (x * signum y) (abs y)
infixl 7  %

reduce :: Int -> Int -> RatioInt
reduce _   0 = ratioZeroDenominatorError
reduce !x !y = let d = gcd x y
               in RatioInt (x `quot` d) (y `quot` d)

-- Instances -------------------------------------------------------------------

instance Ord RatioInt where
    RatioInt x y <= RatioInt x' y' = x * y' <= x' * y
    RatioInt x y <  RatioInt x' y' = x * y' <  x' * y

instance Num RatioInt where
    RatioInt x y + RatioInt x' y' = reduce (x * y' + x' * y) (y * y')
    RatioInt x y - RatioInt x' y' = reduce (x * y' - x' * y) (y * y')
    RatioInt x y * RatioInt x' y' = reduce (x * x')          (y * y')

    negate (RatioInt x y)         = RatioInt (-x)            y
    abs    (RatioInt x y)         = RatioInt (abs x)         y
    signum (RatioInt x _)         = RatioInt (signum x)      1
    fromInteger x                 = RatioInt (fromInteger x) 1

instance Fractional RatioInt where
    (RatioInt x y) / (RatioInt x' y') = (x * y') % (y * x')

    recip (RatioInt 0 _) = ratioZeroDenominatorError
    recip (RatioInt x y)
        | x < 0          = RatioInt (negate y) (negate x)
        | otherwise      = RatioInt x          y

    fromRational r = fromInteger (R.numerator r) % fromInteger (R.denominator r)

instance Real RatioInt where
    toRational (RatioInt x y) = toInteger x R.% toInteger y

instance RealFrac RatioInt where
    properFraction (RatioInt x y) = let (q, r) = x `quotRem` y
                                    in (fromIntegral q, RatioInt r y)
    truncate       (RatioInt x y) = fromIntegral (x `quot` y)

instance Enum RatioInt where
    succ x = x + 1
    pred x = x - 1

    toEnum n = RatioInt n 1
    fromEnum = truncate

    enumFrom       !n       = n : enumFrom (n + 1)
    enumFromThen   !n !m    = n : enumFromThen m (m + m - n)
    enumFromTo     !n !m    = takeWhile (<= m + 1 / 2) (enumFrom n)
    enumFromThenTo !n !m !p =
        takeWhile predicate (enumFromThen n m)
       where
        mid = (m - n) / 2
        predicate | m >= n    = (<= p + mid)
                  | otherwise = (>= p + mid)
