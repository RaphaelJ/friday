{-# LANGUAGE BangPatterns, FlexibleInstances, GADTs, MultiParamTypeClasses
           , TypeFamilies #-}

-- | Provides high level functions to apply filters on images.
module Vision.Image.Filter (
      Filter (..), Kernel (..), FilterInit (..), apply, blur, scharr
    ) where

import Data.Word
import Foreign.Storable (Storable)

import Vision.Image.Type (
      Manifest, MaskedImage (..), Image (..), FromFunction (..)
    )
import Vision.Primitive (Z (..), (:.) (..), DIM1, DIM2, ix2)

data Filter src acc dst = Filter {
      fKernelSize   :: !DIM2
    , fKernelCenter :: !KernelAnchor
    , fKernel       :: !(Kernel     src acc)
    , fInit         :: !(FilterInit src acc)
    , fPost         :: !(acc -> dst)
    , fInterpol     :: !(BorderInterpolate src)
    }

-- | Defines how to center the kernel.
data KernelAnchor = KernelAnchor !DIM2
                  | KernelAnchorCenter

-- | Function which will be applied to every pixel.
-- Some kernels can be factorized in two uni-dimensional kernels (horizontal and
-- vertical). See <http://http://en.wikipedia.org/wiki/Separable_filter>.
data Kernel src acc = Kernel !(DIM2 -> src -> acc -> acc)
                    | SeparableKernel !(DIM1 -> src -> acc -> acc)
                                      !(DIM1 -> acc -> acc -> acc)

data FilterInit src acc where
    FilterFold  :: acc -> FilterInit src acc
    FilterFold1 ::        FilterInit src src

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
class FiltrableImages src dst p where
    -- | Gives the type of the accumulator image given the source and
    -- destination image and the accumulator pixel type.
    type AccumulatorImage src dst p

instance (Storable p_acc) => FiltrableImages src (Manifest p_dst) p_acc where
    type AccumulatorImage src (Manifest p_dst) p_acc = Manifest p_acc

apply :: (Image src, FromFunction dst{-, FiltrableImages src dst acc-})
      => src
      -> Filter (ImagePixel src) acc (FromFunctionPixel dst)
      -> dst
apply !img !(Filter (Z :. kh :. kw) anchor (Kernel kernel) ini post interpol) =
    case ini of FilterFold acc ->
                    fromFunction size $ \(!(Z :. y :. x)) ->
                        post $! goColumn (y - cy) (x - cx) 0 acc
--                 FilterFold1    ->
--                     fromFunction size $ \(!(Z :. y :. x)) ->
--                         post $! goColumn1 (y - cy) (x - cx)
  where
    !size@(Z :. ih :. iw) = shape img

    goColumn !iy !ix !ky !acc
        | ky < kh   =
            case borderInterpolate interpol ih iy of
                Left  iy' -> goLine iy (iy' * iw) ix ix ky 0 acc
                Right val -> goLineConst iy ix ky 0 val acc
        | otherwise = acc

--     goColumn1 iy ix
--         | kh > 0 && kw > 0 =
--             let !ky   = Z :. 0
--                 !kix  = ky :. 0
--                 !linearIY = iy * iw
--                 !acc' = case borderInterpolate interpol ih iy of
--                             Left  iy' -> goLine linearIY (ky :. 1) (ix + 1)
--                                                 (img `linearIndex` linearIY)
--                             Right val -> goLineConst (ky :. 1) val val
--             in goColumn (iy + 1) (linearIY + iw) ix 1 acc'
--         | otherwise = error "Using FilterFold1 with an empty kernel."

    goLine !iy !linearIY !ixs !ix !ky !kx !acc
        | kx < kw   =
            let !val = case borderInterpolate interpol iw ix of
                            Left  ix'  -> img `linearIndex` (linearIY + ix')
                            Right val' -> val'
                !acc' = kernel (ix2 ky kx) val acc
            in goLine iy linearIY ixs (ix + 1) ky (kx + 1) acc'
        | otherwise = goColumn (iy + 1) ixs (ky + 1) acc

    goLineConst !iy !ix !ky !kx !val !acc
        | kx < kw   = let !acc' = kernel (ix2 ky kx) val acc
                      in goLineConst iy ix ky (kx + 1) val acc'
        | otherwise = goColumn (iy + 1) ix (ky + 1) acc

    !(Z :. cy :. cx) = case anchor of KernelAnchor c     -> c
                                      KernelAnchorCenter -> Z :. (kh `quot` 2)
                                                              :. (kw `quot` 2)
apply img (Filter size center (SeparableKernel f1 f2) fold post interpol) =
    undefined
{-# INLINE apply #-}

-- | Given a method of interpolation, the number of pixel in the dimension and
-- an index in this dimension, returns either the index of the interpolated
-- pixel or a constant value.
borderInterpolate :: BorderInterpolate a -> Int -> Int -> Either Int a
borderInterpolate !interpol !len !ix 
    | word ix < word len = Left ix
    | otherwise          =
        case interpol of
            BorderReplicate | ix < 0    -> Left 0
                            | otherwise -> Left $! len - 1
            BorderReflect               -> Left $! goReflect ix
            BorderWrap                  -> Left $! ix `mod` len
            BorderConstant i            -> Right i
  where
    goReflect !ix' | ix' < 0    = goReflect (-ix' - 1)
                   | ix' >= len = goReflect ((len - 1) - (ix' - len))
                   | otherwise  = ix'
{-# INLINE borderInterpolate #-}

-- erode :: Ord input => Filter input input input
-- erode = Filter (ix2 3 3) KernelAnchorCenter
--                (SeparableKernel (const min) (const min)) FilterFold1 id

blur :: Integral a => Int -> Filter a Int a
-- blur radius =
--     Filter (ix2 size size) KernelAnchorCenter (SeparableKernel one one)
--            (FilterFold 0) (\acc -> fromIntegral $ acc `div` size `div` size)
--   where
--     !size = radius * 2 + 1
-- 
--     one _ !v !acc = acc + fromIntegral v
blur radius =
    Filter (ix2 size size) KernelAnchorCenter (Kernel one)
           (FilterFold 0) (\acc -> fromIntegral $ acc `div` size `div` size)
           BorderReplicate
  where
    !size = radius * 2 + 1

    one _ !v !acc = acc + fromIntegral v
    {-# INLINE one #-}
{-# INLINE blur #-}

scharr :: (Integral a, Num b) => Filter a b b
scharr =
    Filter (ix2 3 3) KernelAnchorCenter (Kernel kernel) (FilterFold 0) id
           BorderReplicate
  where
    kernel (Z :. 0 :. 0) val acc = acc - 3  * fromIntegral val
    kernel (Z :. 1 :. 0) val acc = acc - 10 * fromIntegral val
    kernel (Z :. 2 :. 0) val acc = acc - 3  * fromIntegral val
    kernel (Z :. 0 :. 2) val acc = acc + 3  * fromIntegral val
    kernel (Z :. 1 :. 2) val acc = acc + 10 * fromIntegral val
    kernel (Z :. 2 :. 2) val acc = acc + 3  * fromIntegral val
    kernel _             _   acc = acc

word :: Integral a => a -> Word
word = fromIntegral