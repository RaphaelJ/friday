{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs #-}

module Vision.Image.Threshold (
      ThresholdType (..)
    , threshold
    , AdaptiveThresholdKernel (..), adaptiveThreshold
    , otsu
    ) where

import Foreign.Storable (Storable)

import Vision.Image.Filter (Filter (..), SeparableFilter, blur, gaussianBlur)
import Vision.Image.Type (ImagePixel, FunctorImage)
import Vision.Histogram
import Vision.Histogram as H
import Vision.Primitive.Shape (shapeLength)
import qualified Vision.Image.Type as I

import qualified Data.Vector.Storable as V
import qualified Data.Vector as VU

-- | Specifies what to do with pixels matching the threshold predicate.
--
-- @'BinaryThreshold' a b@ will replace matching pixels by @a@ and non-matchings
-- pixels by @b@.
--
-- @'Truncate' a@ will replace matching pixels by @a@.
data ThresholdType src res where
    BinaryThreshold :: res -> res -> ThresholdType src res
    Truncate        :: src        -> ThresholdType src src

-- | Applies the given predicate and threshold policy on the image.
threshold :: FunctorImage src res
          => (ImagePixel src -> Bool)
          -> ThresholdType (ImagePixel src) (ImagePixel res) -> src -> res
threshold !cond !(BinaryThreshold ifTrue ifFalse) !img =
    I.map (\pix -> if cond pix then ifTrue else ifFalse) img
threshold !cond !(Truncate        ifTrue)         !img =
    I.map (\pix -> if cond pix then ifTrue else pix)     img
{-# INLINE threshold #-}

-- | Defines how pixels of the kernel of the adaptive threshold will be
-- weighted.
--
-- With 'MeanKernel', pixels of the kernel have the same weight.
--
-- With @'GaussianKernel' sigma@, pixels are weighted according to their distance
-- from the thresholded pixel using a Gaussian function parametred by @sigma@.
-- See 'gaussianBlur' for details.
data AdaptiveThresholdKernel acc where
    MeanKernel     :: Integral acc => AdaptiveThresholdKernel acc
    GaussianKernel :: (Floating acc, RealFrac acc)
                   => Maybe acc -> AdaptiveThresholdKernel acc

-- | Applies a thresholding adaptively.
--
-- Compares every pixel to its surrounding ones in the kernel of the given
-- radius.
adaptiveThreshold :: (Integral src, Num src, Ord src, Storable acc)
                  => AdaptiveThresholdKernel acc
                  -> Int -- ^ Kernel radius.
                  -> src -- ^ Minimum difference between the pixel and the
                         -- kernel average. The pixel is thresholded if
                         -- @pixel_value - kernel_mean > difference@ where
                         -- difference if this number. Can be negative.
                  -> ThresholdType src res -> SeparableFilter src acc res
adaptiveThreshold !kernelType !radius !thres !thresType =
    kernelFilter { fPost = post }
  where
    !kernelFilter =
        case kernelType of MeanKernel         -> blur         radius
                           GaussianKernel sig -> gaussianBlur radius sig

    post !pix !acc =
        let !acc' = (fPost kernelFilter) pix acc
            !cond = (pix - acc') > thres
        in case thresType of
                BinaryThreshold ifTrue ifFalse -> if cond then ifTrue
                                                          else ifFalse
                Truncate        ifTrue         -> if cond then ifTrue else pix
{-# INLINE adaptiveThreshold #-}

-- | Applies a clustering-based image thresholding using the Otsu's method.
--
-- See <https://en.wikipedia.org/wiki/Otsu's_method>.
otsu :: ( HistogramShape (PixelValueSpace (ImagePixel src))
        , ToHistogram (ImagePixel src), FunctorImage src res
        , Ord (ImagePixel src), Num (ImagePixel src), Enum (ImagePixel src))
     => ThresholdType (ImagePixel src) (ImagePixel res) -> src -> res
otsu !thresType !img =
    threshold (<= thresh) thresType img
 where
    !thresh =
        let hist       = histogram Nothing img
            histV      = H.vector hist
            tot        = shapeLength (I.shape img)
            runningMul = V.zipWith (\v i -> v * i) histV (V.fromList [0..255])
            sm         = double (V.sum $ V.drop 1 runningMul)
            wB         = V.scanl1 (+) histV
            wF         = V.map (\x -> tot - x) wB
            sumB       = V.scanl1 (+) runningMul
            mB         = V.zipWith (\n d -> if d == 0 then 1
                                                      else double n / double d)
                                   sumB wB
            mF         = V.zipWith (\b f -> if f == 0 then 1
                                                      else   (sm - double b)
                                                           / double f)
                                   sumB wF
            between    = V.zipWith4 (\x y b f ->   double x * double y
                                                 * (b - f)^two)
                                    wB wF mB mF
        in snd $ VU.maximum (VU.zip (VU.fromList $ V.toList between)
                                    (VU.fromList [0..255]))

    !two    = 2 :: Int
{-# INLINABLE otsu #-}

double :: Integral a => a -> Double
double = fromIntegral
