{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Vision.Histogram (tests) where

import Control.Applicative ((<$>))
import Data.Int
import qualified Data.Vector.Storable as V
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Positive, getPositive)

import Vision.Histogram
import Vision.Image (Grey)
import qualified Vision.Image as I
import Vision.Primitive (Z (..), (:.) (..), DIM1, ix1, ix3)
import Test.Vision.Image ()

instance (Arbitrary (Positive p), Bounded p, Integral p, V.Storable p)
    => Arbitrary (Histogram DIM1 p) where
    arbitrary = do
        let !maxVal = maxBound `quot` 256 -- Sum musn't overflow.
        vec <- V.replicateM 256 (getPositive <$> arbitrary)
        return $ Histogram (Z :. 256) $ V.map (`rem` maxVal) vec

tests :: [Test]
tests = [
      testProperty "Sum of bins equals the number of pixels" propCalcHist

    , testProperty "The reduction of a 2D histogram gives the linear one."
                   propReduceHist

    , testProperty "Resizing an histogram equals the computation of the \
                   \smallest one."
                   propResizeHist

    , testProperty "Cumulative histogram last bin equals original's sum"
                   propCumulatHist

    , testProperty "Sum of an normalized histogram equals its size"
                   propNormalizedHist

    , testProperty "Comparing the same histogram returns a perfect correlation"
                   propCorrelation
    , testProperty "Comparing the same histogram returns a 0 chi-square value"
                   propChiSquare
    , testProperty "Comparing the same histogram returns an intersection value \
                   \equals to the sum of the values of the histogram"
                   propIntersec
    , testProperty "Comparing the same histogram returns a 0 EMD value"
                   propEMD
    ]

-- | The sum of the values of the histogram is equal to the number of pixels of
-- the image.
propCalcHist :: Grey -> Bool
propCalcHist img =
    let Z :. h :. w     = I.shape img
        Histogram _ vec = histogram Nothing img
    in V.sum vec == w * h

-- | Checks the identity @histogram == reduce . histogram2D@.
propReduceHist :: Grey -> Bool
propReduceHist img =
    let hist1 = histogram Nothing img :: Histogram DIM1 Int32
        hist2 = reduce (histogram2D (ix3 256 3 3) img)
    in hist1 == hist2

-- | Checks the resizing of an histogram equals the direct computation of the
-- smallest one.
propResizeHist :: Grey -> Bool
propResizeHist img =
    let hist1 = histogram (Just (ix1 128)) img :: Histogram DIM1 Int32
        hist2 = resize (ix1 128) (histogram (Just (ix1 256)) img)
    in hist1 == hist2

-- | Checks if the last bin of the cumulative histogram equals the sum of the
-- values of the original histogram.
propCumulatHist :: Histogram DIM1 Int32 -> Bool
propCumulatHist hist@(Histogram _ vec) =
    let Histogram _ vec' = cumulative hist
    in V.sum vec == vec' V.! 255

-- | Checks that the sums of an equalized histogram equals the desired value.
propNormalizedHist :: Double -> Histogram DIM1 Int32 -> Bool
propNormalizedHist val hist =
    let Histogram _ vec = normalize val hist
    in round (V.sum vec) == (round val :: Integer)

-- | Checks that the comparison of two identical histograms gives the
-- correlation value 1.
propCorrelation :: Histogram DIM1 Int32 -> Bool
propCorrelation hist = round (compareCorrel hist hist :: Double) == (1 :: Int)

-- | Checks that the comparison of two identical histograms gives the zero
-- Chi-square value.
propChiSquare :: Histogram DIM1 Int32 -> Bool
propChiSquare hist = round (compareChi hist hist :: Double) == (0 :: Int)

-- | Checks that the comparison of two identical histograms gives the sum of the
-- values of the histogram.
propIntersec :: Histogram DIM1 Int32 -> Bool
propIntersec hist@(Histogram _ vec) = compareIntersect hist hist == V.sum vec

-- | Checks that the comparison of two identical histograms gives a zero
-- EMD value.
propEMD :: Histogram DIM1 Int32 -> Bool
propEMD hist = compareEMD hist hist == 0
