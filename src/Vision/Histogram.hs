{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

-- | Contains functions to compute and manipulate histograms as well as some
-- images transformations which are histograms-based.
-- Every polymorphic function is specialised for histograms of 'Int32', 'Double'
-- and 'Float'.
module Vision.Histogram (
    -- * Types
      Histogram (..)
    -- * Histogram computations
    , calcHist{-, cumulatHist, roundHist, equalizeHist
    -- * Histogram comparisons
    , compareCorrel, compareChi, compareIntersect, compareLogLikelihood
    -- * Images processing
    , equalizeImage-}
    ) where

import Data.Int
import Data.Vector.Storable (Vector, (!), unsafeAccumulate_, create, enumFromN, forM_)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Storable (Storable)

import Vision.Image (Image (..), Size (..), GreyImage, GreyPixel (..), Point (..))

newtype Histogram a = Histogram { histVector :: Vector a }
    deriving (Eq, Show)

-- | Computes an histogram from a grey scale image. The index range of the
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
{-# SPECIALIZE calcHist :: GreyImage -> Histogram Int32 #-}
{-# SPECIALIZE calcHist :: GreyImage -> Histogram Double #-}
{-# SPECIALIZE calcHist :: GreyImage -> Histogram Float #-}

-- -- | Computes the cumulative histogram of another histogram.
-- -- C(i) = SUM H(j) for each j in [0..i] where C is the cumulative histogram, and
-- -- H the original histogram.
-- cumulatHist :: (IArray UArray a, Num a) => Histogram a -> Histogram a
-- cumulatHist hist = listArray (bounds hist) $ scanl1 (+) $ elems hist
-- {-# SPECIALIZE cumulatHist :: Histogram Int32 -> Histogram Int32 #-}
-- {-# SPECIALIZE cumulatHist :: Histogram Double -> Histogram Double #-}
-- {-# SPECIALIZE cumulatHist :: Histogram Float -> Histogram Float #-}
-- 
-- -- | Rounds each values of the histogram to its nearest integer.
-- roundHist :: (IArray UArray a, RealFrac a, IArray UArray b, Integral b) =>
--              Histogram a -> Histogram b
-- roundHist hist = listArray (bounds hist) $ map round $ elems hist
-- {-# SPECIALIZE roundHist :: Histogram Double -> Histogram Int32 #-}
-- {-# SPECIALIZE roundHist :: Histogram Float -> Histogram Int32 #-}
-- 
-- -- | Normalizes the histogram so that the sum of histogram bins is equals to the
-- -- number of bins in the histogram.
-- -- See <http://en.wikipedia.org/wiki/Histogram_equalization>.
-- equalizeHist :: (IArray UArray a, Integral a, IArray UArray b, Fractional b) =>
--                 Histogram a -> Histogram b
-- equalizeHist hist =
--     let b = bounds hist
--         !maxIx = fromIntegral $ snd b
--         !n = fromIntegral $ sum $ elems hist
--         equalizeVal ix = fromIntegral (hist ! ix) / n * maxIx
--     in listArray b [ equalizeVal i | i <- range b ]
-- {-# SPECIALIZE equalizeHist :: Histogram Int32 -> Histogram Double #-}
-- {-# SPECIALIZE equalizeHist :: Histogram Int32 -> Histogram Float #-}
-- 
-- -- TODO: Comparison operator mus be applied on floating point histograms
-- 
-- -- | Computes the Pearson\'s correlation coefficient
-- -- between each corresponding bins of the two histograms.
-- -- A value of 1 implies a perfect correlation, a value of -1 a perfect
-- -- opposition and a value of 0 no correlation at all.
-- -- See <http://en.wikipedia.org/wiki/Pearson_correlation_coefficient>.
-- compareCorrel :: (IArray UArray a, Integral a) =>
--                  Histogram a -> Histogram a -> Double
-- compareCorrel hist1 hist2 =
--     numerat / denominat
--   where
--     numerat = sum [
--           (double v1 - avg1) * (double v2 - avg2)
--         | v1 <- elems hist1 | v2 <- elems hist2
--         ]
--     denominat = sqrt $ sum [ (double v1 - avg1)**2 | v1 <- elems hist1 ] *
--                        sum [ (double v2 - avg2)**2 | v2 <- elems hist2 ]
--     (avg1, avg2) = (avg hist1, avg hist2)
-- 
--     avg hist = double (sum (elems hist)) / double (len hist)
--     len = rangeSize . bounds
-- {-# SPECIALIZE compareCorrel :: Histogram Int32 -> Histogram Int32 -> Double #-}
-- --{-# SPECIALIZE compareCorrel :: Histogram Int32 -> Histogram Int32 -> Float #-}
-- 
-- -- | Computes the Chi-squared distance between two histograms.
-- -- A value of 0 indicates a perfect match.
-- -- d(i) = (H1(i) - H2(i))^2 / (H1(i) + H2(i))
-- -- chi = SUM (i) for each indice i the histograms.
-- compareChi :: (IArray UArray a, Fractional a, Eq a) =>
--                Histogram a -> Histogram a -> a
-- compareChi hist1 hist2 = sum [
--       if v1 == v2 then 0 else square (v1 - v2) / (v1 + v2)
--     | v1 <- elems hist1 | v2 <- elems hist2
--     ]
--   where
--     square v = v * v
-- {-# SPECIALIZE compareChi :: Histogram Double -> Histogram Double -> Double #-}
-- {-# SPECIALIZE compareChi :: Histogram Float -> Histogram Float-> Float #-}
-- 
-- -- | Computes the intersection of the two histograms.
-- -- The higher the score is, the best the match is.
-- -- intersec = SUM min(H1(i), H2(i)) for each indice i the histograms.
-- compareIntersect :: (IArray UArray a, Num a, Ord a) =>
--                     Histogram a -> Histogram a -> a
-- compareIntersect hist1 hist2 =
--     sum [ min v1 v2 | v1 <- elems hist1 | v2 <- elems hist2 ]
-- {-# SPECIALIZE compareIntersect ::
--     Histogram Int32 -> Histogram Int32 -> Int32 #-}
-- {-# SPECIALIZE compareIntersect ::
--     Histogram Double -> Histogram Double -> Double #-}
-- {-# SPECIALIZE compareIntersect ::
--     Histogram Float -> Histogram Float -> Float #-}
-- 
-- -- | Computes the Log-likelihood distance between two histograms.
-- -- log-likelihood = SUM (H1(i) * log (H2(i)))
-- compareLogLikelihood :: (IArray UArray a, Integral a) =>
--                         Histogram a -> Histogram a -> Double
-- compareLogLikelihood hist1 hist2 = - sum [
--           (double v1) * log (double v2)
--         | v1 <- elems hist1 | v2 <- elems hist2
--         ]
-- {-# SPECIALIZE compareLogLikelihood ::
--     Histogram Int32 -> Histogram Int32 -> Double #-}
-- 
-- -- | Normalizes a grey scale image by equalising the histogram.
-- -- The algorithm normalizes the brightness and increases the contrast of the
-- -- image by mapping each pixels values to the value at the index of the
-- -- cumulative equalized histogram :
-- -- N(x, y) = H(I(x, y)) where N is the equalized image, I is the image and H the
-- -- cumulative equalized histogram.
-- equalizeImage :: GreyImage -> GreyImage
-- equalizeImage image =
--     fromFunction (getSize image) equalizePixel
--   where
--     hist :: Histogram Int32
--     hist = calcHist image
--     cumEqHist :: Histogram Double
--     cumEqHist = cumulatHist $! equalizeHist hist
--     roundedCumEqHist = roundHist cumEqHist
--     equalizePixel pt =
--         let val = image `unsafeGetPixel` pt
--         in roundedCumEqHist ! (int val)

int:: Integral a => a -> Int
int = fromIntegral
double :: Integral a => a -> Double
double = fromIntegral
