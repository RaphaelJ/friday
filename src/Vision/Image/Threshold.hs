{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs #-}

module Vision.Image.Threshold (
      ThresholdType (..), thresholdType
    , threshold
    , AdaptiveThresholdKernel (..), AdaptiveThreshold, adaptiveThreshold
    , otsu
    , scw
    ) where

import Data.Int
import Foreign.Storable (Storable)

import Vision.Image.Filter (
      Filter (..), BoxFilter, SeparableFilter, Kernel (..)
    , KernelAnchor (KernelAnchorCenter), FilterFold (..)
    , BorderInterpolate (BorderReplicate)
    , apply, blur, gaussianBlur, Mean, mean)
import Vision.Image.Type (
      Image, ImagePixel, FromFunction (..), FunctorImage, Manifest
    , (!), shape, delayed, manifest
    )
import Vision.Histogram (
      HistogramShape, PixelValueSpace, ToHistogram, histogram
    )
import qualified Vision.Histogram as H
import Vision.Primitive (Z (..), (:.) (..), Size, shapeLength)
import qualified Vision.Image.Type as I

import qualified Data.Vector.Storable as V
import qualified Data.Vector as VU

-- | Specifies what to do with pixels matching the threshold predicate.
--
-- @'BinaryThreshold' a b@ will replace matching pixels by @a@ and non-matchings
-- pixels by @b@.
--
-- @'Truncate' a@ will replace matching pixels by @a@.
--
-- @'TruncateInv' a@ will replace non-matching pixels by @a@.
data ThresholdType src res where
    BinaryThreshold :: res -> res -> ThresholdType src res
    Truncate        :: src        -> ThresholdType src src
    TruncateInv     :: src        -> ThresholdType src src

-- | Given the thresholding method, a boolean indicating if the pixel match the
-- thresholding condition and the pixel, returns the new pixel value.
thresholdType :: ThresholdType src res -> Bool -> src -> res
thresholdType (BinaryThreshold ifTrue ifFalse) match _   | match     = ifTrue
                                                         | otherwise = ifFalse
thresholdType (Truncate        ifTrue)         match pix | match     = ifTrue
                                                         | otherwise = pix
thresholdType (TruncateInv     ifFalse)        match pix | match     = pix
                                                         | otherwise = ifFalse
{-# INLINE thresholdType #-}

-- -----------------------------------------------------------------------------

-- | Applies the given predicate and threshold policy on the image.
threshold :: FunctorImage src res
          => (ImagePixel src -> Bool)
          -> ThresholdType (ImagePixel src) (ImagePixel res) -> src -> res
threshold !cond !thresType =
    I.map (\pix -> thresholdType thresType (cond pix) pix)
{-# INLINE threshold #-}

-- -----------------------------------------------------------------------------

type AdaptiveThreshold src acc res = SeparableFilter src () acc res

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

-- | Creates an adaptive thresholding filter.
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
                  -> ThresholdType src res -> AdaptiveThreshold src acc res
adaptiveThreshold !kernelType !radius !thres !thresType =
    kernelFilter { fPost = post }
  where
    !kernelFilter =
        case kernelType of MeanKernel         -> blur         radius
                           GaussianKernel sig -> gaussianBlur radius sig

    post ix pix ini acc =
        let !acc' = (fPost kernelFilter) ix pix ini acc
            !cond = (pix - acc') > thres
        in thresholdType thresType cond pix
{-# INLINE adaptiveThreshold #-}

-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------

-- | This is a sliding concentric window filter (SCW) that uses the ratio of the
-- standard deviations of two sliding windows centered on a same point to detect
-- regions of interest (ROI).
--
-- > scw sizeWindowA sizeWindowB beta thresType img
--
-- Let @σA@ be the standard deviation of the window A around a pixel and @σB@
-- be the standard deviation of another window around the same pixel.
-- Then the pixel will match the threshold if @σB / σA >= beta@, and will be
-- thresholded according to the given 'ThresholdType'.
--
-- See <http://www.academypublisher.com/jcp/vol04/no08/jcp0408771777.pdf>
scw :: ( Image src, Integral (ImagePixel src), FromFunction dst
       , Floating stdev, Fractional stdev, Ord stdev, Storable stdev)
    => Size -> Size -> stdev
    -> ThresholdType (ImagePixel src) (FromFunctionPixel dst) -> src -> dst
scw !sizeA !sizeB !beta !thresType !img =
    betaThreshold (stdDev sizeA) (stdDev sizeB)
  where
    betaThreshold a b =
        fromFunction (shape img) $ \pt ->
            let !cond = (b ! pt) / (a ! pt) < beta
            in thresholdType thresType cond (img ! pt)

    stdDev size =
       let filt :: (Integral src, Fractional res) => Mean src Int16 res
           filt     = mean size
           !meanImg = manifest $ apply filt img
           !varImg  = manifest $ apply (variance size meanImg) img
       in delayed $ I.map sqrt varImg
{-# INLINABLE scw #-}

-- | Given a mean image and an original image, computes the variance of the
-- kernel of the given size.
--
-- @average [ (origPix - mean)^2 | origPix <- kernel pixels on original ]@.
variance :: (Integral src, Fractional res, Storable res)
         => Size -> Manifest res -> BoxFilter src res res res
variance !size@(Z :. h :. w) !meanImg =
    Filter size KernelAnchorCenter (Kernel kernel) (\pt _ -> meanImg ! pt)
           (FilterFold (const 0)) post BorderReplicate
  where
    kernel !kernelMean _ !val !acc =
        acc + square (fromIntegral val - kernelMean)

    !nPixsFactor = 1 / (fromIntegral $! h * w)
    post _ _ _ !acc  = acc * nPixsFactor
{-# INLINABLE variance #-}

-- -----------------------------------------------------------------------------

square :: Num a => a -> a
square a = a * a

double :: Integral a => a -> Double
double = fromIntegral
