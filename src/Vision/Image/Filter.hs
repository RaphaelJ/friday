{-# LANGUAGE BangPatterns, GADTs #-}

-- | Provides high level functions to apply filters on images.
module Vision.Image.Filter (
      Filter (..), Kernel (..), FilterFold (..), apply
    ) where

import Vision.Image.Interpolate (Interpolable, bilinearInterpol)
import Vision.Image.Type (MaskedImage (..), Image (..), FromFunction (..))
import Vision.Primitive (Z (..), (:.) (..), RPoint (..), Rect (..), Size, DIM1, DIM2, ix2)

data Filter input acc output = Filter {
      fKernelSize   :: !DIM2
    , fKernelCenter :: !KernelAnchor
    , fKernel       :: !(Kernel     input acc)
    , fFold         :: !(FilterFold input acc)
    , fPost         :: !(acc -> output)
    }

-- | Defines how to center the kernel.
data KernelAnchor = KernelAnchor !DIM2
                  | KernelAnchorCenter

-- | Function which will be applied to every pixel.
-- Some kernels can be factorized in two uni-dimensional kernels (horizontal and
-- vertical). See <http://http://en.wikipedia.org/wiki/Separable_filter>.
data Kernel input acc = Kernel !(DIM2 -> input -> acc -> acc)
                      | SeparableKernel !(DIM1 -> input -> acc -> acc)
                                        !(DIM1 -> acc   -> acc -> acc)

data FilterFold input acc where
    FilterFold  :: acc -> FilterFold input acc
    FilterFold1 ::        FilterFold input input

-- | Defines how image boundaries are extrapolated by the algorithms.
-- '|' characters in examples are image borders.
data BorderInterpolate a =
    -- | Replicates the first and last pixels of the image.
    -- > aaaaaa|abcdefgh|hhhhhhh
      BorderReplicate
    -- | Reflects the border of the image.
    -- > fedcba|abcdefgh|hgfedcb
    | BorderReflect
    -- | Considers that the last pixel of the image is before the first one.
    -- > cdefgh|abcdefgh|abcdefg
    | BorderWrap
    -- | Assigns a constant value to out of image pixels.
    -- > iiiiii|abcdefgh|iiiiiii  with some specified 'i'
    | BorderConstant !a

-- | Defines what image will be used as accumulator when filtering using a
-- separable kernel.
class (acc ~ AccumulatorImage src dst, Image acc, FromFunction acc)
    => FiltrableImage src dst p where
    -- | Gives the type of the accumulator image given the source and
    -- destination image and the accumulator pixel type.
    type AccumulatorImage src dst p

instance (Storable p_acc) => FiltrableImage src (Manifest p_dst) p_acc where
    type AccumulatorImage src (Manifest p_dst) p_acc = Manifest p_acc

apply :: (Image src, FromFunction dst, FiltrableImage src dst)
      => src
      -> Filter (ImagePixel src) acc (FromFunctionPixel dst)
      -> BorderInterpolate (ImagePixel src)
      -> dst
apply !img !(Filter (Z :. kh :. kw) anchor (Kernel kernel) fold post) =
    

    !center = case anchor of KernelAnchor c     -> c
                             KernelAnchorCenter -> Z :. (kh `div` 2)
                                                     :. (kw `div` 2)

    accum :: img -> AccumulatorImage img
    accum = 
apply img (Filter size center (SeparableKernel f) fold post) =
--     fromFunction (shape img) $ \(Z :. y :. x) ->
--         post 
-- 
--   where
--     center' | 

-- | Given a method of interpolation, the number of pixel in the dimension and
-- an index in this dimension, returns either the index of the interpolated
-- pixel or a constant value.
borderInterpolate :: BorderInterpolate a -> Int -> Int -> Either Int a
borderInterpolate !interpol !maxIx !ix | word ix < word maxIx = Left ix
                                       | otherwise            =
    case interpol of
        BorderReplicate | ix < 0    -> Left 0
                        | otherwise -> Left $! maxIx - 1
        BorderReflect               -> Left $! goReflect ix
        BorderWrap                  -> Left $! ix `mod` maxIx
        BorderConstant i            -> Right i
  where
    goReflect !ix' | ix' < 0      = goReflect (-ix' - 1)
                   | ix' >= maxIx = goReflect ((maxIx - 1) - (ix' - maxIx))
                   | otherwise    = ix'
{-# INLINE borderInterpolate #-}

erode :: Ord input => Filter input input input
erode = Filter (ix2 3 3) KernelAnchorCenter
               (SeparableKernel (const min) (const min)) FilterFold1 id

blur :: Integral a => Int -> Filter a Int a
blur radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel one one)
           (FilterFold 0) (\acc -> fromIntegral $ acc `div` size `div` size)
  where
    !size = radius * 2 + 1

    one _ !v !acc = acc + fromIntegral v