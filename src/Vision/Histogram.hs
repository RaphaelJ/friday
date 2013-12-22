{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains functions to compute and manipulate histograms as well as some
-- images transformations which are histograms-based.
-- 
-- Every polymorphic function is specialised for histograms of 'Int32', 'Double'
-- and 'Float'. Other types could be specialized as every polymorphic functions
-- are declared @INLINABLE@.
module Vision.Histogram (
    -- * Types
      Histogram (..)
    -- * Histogram computations
    , calcHist, cumulatHist, roundHist, normalizeHist
    -- * Images processing
    , equalizeImage
    -- * Histogram comparisons
    , compareCorrel, compareChi, compareIntersect, compareMatch
    ) where

import Data.Int
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)

import Vision.Image (Image (..), FromFunction (..), Size (..), GreyImage)
import qualified Vision.Image as I

-- There is no rule to simplify the conversion from Int32 to Double and Float
-- when using realToFrac. Both conversions are using a temporary yet useless
-- Rational value.

{-# RULES
"realToFrac/Int32->Double" realToFrac = fromIntegral :: Int32 -> Double
"realToFrac/Int32->Float"  realToFrac = fromIntegral :: Int32 -> Float
  #-}

newtype Histogram a = Histogram { histVector :: Vector a }
    deriving (Eq, Show)

-- | Computes an histogram from a single channel image. The index range of the
-- histogram is [0; 255].
calcHist :: (Image i, Integral (ImagePixel i), Storable a, Num a)
         => i -> Histogram a
calcHist img =
    let Size w h = getSize img
        !n       = w * h
        zeros    = V.replicate 256 0
        ones     = V.replicate n 1
        pixVals  = V.map int (getVector img)
    in Histogram $ V.accumulate_ (+) zeros pixVals ones
{-# SPECIALIZE calcHist :: GreyImage -> Histogram Int32
                        ,  GreyImage -> Histogram Double
                        ,  GreyImage -> Histogram Float #-}
{-# INLINABLE calcHist #-}

-- | Computes the cumulative histogram of another histogram.
-- C(i) = SUM H(j) for each j in [0..i] where C is the cumulative histogram, and
-- H the original histogram.
cumulatHist :: (Storable a, Num a) => Histogram a -> Histogram a
cumulatHist = Histogram . V.scanl1' (+) . histVector
{-# SPECIALIZE cumulatHist :: Histogram Int32  -> Histogram Int32
                           ,  Histogram Double -> Histogram Double
                           ,  Histogram Float  -> Histogram Float #-}
{-# INLINABLE cumulatHist #-}

-- | Rounds each values of the histogram to its nearest integer.
roundHist :: (Storable a, RealFrac a, Storable b, Integral b)
          => Histogram a -> Histogram b
roundHist = Histogram . V.map round . histVector
{-# SPECIALIZE roundHist :: Histogram Double -> Histogram Int32
                         ,  Histogram Float  -> Histogram Int32 #-}
{-# INLINABLE roundHist #-}

-- | Normalizes the histogram so that the sum of histogram bins is equal to the
-- number of bins in the histogram, minus 1.
-- See <http://en.wikipedia.org/wiki/Histogram_equalization>.
normalizeHist :: (Storable a, Real a, Storable b, Fractional b)
             => Histogram a -> Histogram b
normalizeHist !(Histogram vec) =
    let !maxIx = fromIntegral (V.length vec - 1)
        !n     = realToFrac (V.sum vec)
        !ratio = maxIx / n
        equalizeVal !val = realToFrac val * ratio
        {-# INLINE equalizeVal #-}
    in Histogram $ V.map equalizeVal vec
{-# SPECIALIZE normalizeHist :: Histogram Int32  -> Histogram Double
                             ,  Histogram Int32  -> Histogram Float
                             ,  Histogram Double -> Histogram Double
                             ,  Histogram Double -> Histogram Float
                             ,  Histogram Float  -> Histogram Float
                             ,  Histogram Float  -> Histogram Double #-}
{-# INLINABLE normalizeHist #-}

-- | Equalizes a grey scale image by equalising the histogram.
-- The algorithm equalizes the brightness and increases the contrast of the
-- image by mapping each pixels values to the value at the index of the
-- cumulative norm histogram :
-- N(x, y) = H(I(x, y)) where N is the equalized image, I is the image and H the
-- cumulative normalized histogram.
equalizeImage :: (Integral (ImagePixel i1), Image i1
                 , Num (ImagePixel i2), FromFunction i2) => i1 -> i2
equalizeImage img =
    I.map equalizePixel img
  where
    hist          = calcHist img                     :: Histogram Int32
    cumNormHist   = cumulatHist $ normalizeHist hist :: Histogram Double
    !cumNormHist' = roundHist cumNormHist            :: Histogram Int32
    equalizePixel !val = fromIntegral $ histVector cumNormHist' ! (int val)
{-# SPECIALIZE equalizeImage :: GreyImage -> GreyImage #-}
{-# INLINABLE equalizeImage #-}

-- | Computes the Pearson\'s correlation coefficient
-- between each corresponding bins of the two histograms.
-- A value of 1 implies a perfect correlation, a value of -1 a perfect
-- opposition and a value of 0 no correlation at all.
-- See <http://en.wikipedia.org/wiki/Pearson_correlation_coefficient>.
compareCorrel :: (Storable a, Real a, Storable b, Eq b, Floating b)
              => Histogram a -> Histogram a -> b
compareCorrel (Histogram vec1) (Histogram vec2)
    | denominat == 0 = 1
    | otherwise      = numerat / denominat
  where
    numerat   = V.sum $ V.zipWith (*) diff1 diff2
    denominat = sqrt $ V.sum (V.map square diff1) * V.sum (V.map square diff2)

    !diff1 = V.map (\v1 -> realToFrac v1 - avg1) vec1
    !diff2 = V.map (\v2 -> realToFrac v2 - avg2) vec2

    (!avg1, !avg2) = (avg vec1, avg vec2)
    avg vec = realToFrac (V.sum vec) / realToFrac (V.length vec)
{-# SPECIALIZE compareCorrel :: Histogram Int32  -> Histogram Int32  -> Double
                             ,  Histogram Int32  -> Histogram Int32  -> Float
                             ,  Histogram Double -> Histogram Double -> Double
                             ,  Histogram Double -> Histogram Double -> Float
                             ,  Histogram Float  -> Histogram Float  -> Double
                             ,  Histogram Float  -> Histogram Float  -> Float
                             #-}
{-# INLINABLE compareCorrel #-}

-- | Computes the Chi-squared distance between two histograms.
-- A value of 0 indicates a perfect match.
-- d(i) = (H1(i) - H2(i))^2 / ((H1(i) + H2(i)) / 2)
-- chi = SUM (d(i)) for each indice i of the histograms.
compareChi :: (Storable a, Real a, Storable b, Fractional b)
           => Histogram a -> Histogram a -> b
compareChi (Histogram vec1) (Histogram vec2) =
    (V.sum $ V.zipWith f vec1 vec2) / 2
  where
    f v1 v2 | v2 == 0   = 0
            | otherwise = realToFrac (square (v1 - v2)) / realToFrac (v1 + v2)
{-# SPECIALIZE compareChi :: Histogram Int32  -> Histogram Int32  -> Double
                          ,  Histogram Int32  -> Histogram Int32  -> Float
                          ,  Histogram Double -> Histogram Double -> Double
                          ,  Histogram Double -> Histogram Double -> Float
                          ,  Histogram Float  -> Histogram Float  -> Double
                          ,  Histogram Float  -> Histogram Float  -> Float #-}
{-# INLINABLE compareChi #-}

-- | Computes the intersection of the two histograms.
-- The higher the score is, the best the match is.
-- intersec = SUM min(H1(i), H2(i)) for each indice i the histograms.
compareIntersect :: (Storable a, Num a, Ord a) 
                 => Histogram a -> Histogram a -> a
compareIntersect (Histogram vec1) (Histogram vec2) =
    V.sum $ V.zipWith min vec1 vec2
{-# SPECIALIZE compareIntersect ::
       Histogram Int32  -> Histogram Int32  -> Int32
    ,  Histogram Double -> Histogram Double -> Double
    ,  Histogram Float  -> Histogram Float  -> Float #-}
{-# INLINABLE compareIntersect #-}

compareMatch hist1 hist2 =
    let Histogram vec1 = cumulatHist hist1
        Histogram vec2 = cumulatHist hist2
    in V.sum $ V.zipWith (\v1 v2 -> abs (v1 - v2)) vec1 vec2
{-# SPECIALIZE compareMatch ::
       Histogram Int32  -> Histogram Int32  -> Int32
    ,  Histogram Double -> Histogram Double -> Double
    ,  Histogram Float  -> Histogram Float  -> Float #-}
{-# INLINABLE compareMatch #-}

square :: Num a => a -> a
square a = a * a

int :: Integral a => a -> Int
int = fromIntegral
