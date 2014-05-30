{-# LANGUAGE BangPatterns, GADTs #-}

module Vision.Image.Threshold (
      ThresholdType (..)
    , threshold
    , AdaptiveThresholdKernel (..), adaptiveThreshold
    ) where

import Foreign.Storable (Storable)

import Vision.Image.Filter (Filter (..), SeparableFilter, blur, gaussianBlur)
import Vision.Image.Type (ImagePixel, FunctorImage)
import qualified Vision.Image.Type as I

-- | Specifies what to do with pixels matching the threshold predicate.
data ThresholdType src res where
    BinaryThreshold :: res -- ^ Pixel value if the predicate matches.
                    -> res -- ^ Pixel value if the predicate doesn't
                           -- match.
                    -> ThresholdType src res
    -- | Replaces the pixel by the given value only when the predicate matches.
    Truncate        :: src -> ThresholdType src src

-- | Applies the given predicate and threshold policy on the image.
threshold :: FunctorImage src res
          => (ImagePixel src -> Bool)
          -> ThresholdType (ImagePixel src) (ImagePixel res) -> src -> res
threshold !cond !(BinaryThreshold ifTrue ifFalse) !img =
    I.map (\pix -> if cond pix then ifTrue else ifFalse) img
threshold !cond !(Truncate        ifTrue)         !img =
    I.map (\pix -> if cond pix then ifTrue else pix)     img
{-# INLINE threshold #-}

data AdaptiveThresholdKernel acc where
    -- | Each pixel of the kernel has the same weight.
    MeanKernel     :: Integral acc => AdaptiveThresholdKernel acc
    -- | Pixels are weighted according to their distance from the thresholded
    -- pixel using a Gaussian function.
    GaussianKernel :: (Floating acc, RealFrac acc)
                   -- | Sigma value of the Gaussian function.
                   -- See 'gaussianBlur' for details.
                   => Maybe acc
                   -> AdaptiveThresholdKernel acc

-- | Applies a thresholding adaptively.
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
