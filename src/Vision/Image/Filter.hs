{-# LANGUAGE BangPatterns, GADTs #-}

-- | Provides high level functions to apply filters on images.
module Vision.Image.Filter (
      Filter (..), Kernel (..), FilterFold (..), apply
    ) where

import Vision.Image.Interpolate (Interpolable, bilinearInterpol)
import Vision.Image.Type (MaskedImage (..), Image (..), FromFunction (..))
import Vision.Primitive (Z (..), (:.) (..), RPoint (..), Rect (..), Size, DIM1, DIM2, ix2)

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
    FilterFold  :: acc -> FilterFold src acc
    FilterFold1 ::        FilterFold src src

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
    => FiltrableImages src dst p where
    -- | Gives the type of the accumulator image given the source and
    -- destination image and the accumulator pixel type.
    type AccumulatorImage src dst p

instance (Storable p_acc) => FiltrableImage src (Manifest p_dst) p_acc where
    type AccumulatorImage src (Manifest p_dst) p_acc = Manifest p_acc

apply :: (Image src, FromFunction dst, FiltrableImages src dst acc)
      => src
      -> Filter (ImagePixel src) acc (FromFunctionPixel dst)
      -> dst
apply !img !(Filter (Z :. kh :. kw) anchor (Kernel kernel) ini post interpol) =
    case ini of FilterFold acc ->
                    fromFunction size $ \!(Z :. y :. x) ->
                        let !iy = y - cy
                        in goColumn iy (iy * iw) (x - cx) 0 acc
                FilterFold1    ->
                    fromFunction size $ \!(Z :. y :. x) ->
                        goColumn1 (y - cy) (x - cx) 0
  where
    !size@(Z :. ih :. iw) = shape img

    goColumn iy linearIY ix ky acc =
        | ky < kh   =
            case borderInterpolate interpol ih iy of
                Left  iy' -> let !acc' = goLine linearIY (ky :. 0) ix acc
                             in goColumn (iy + 1) (linearIY + iw) ix (ky + 1)
                                         acc'
                Right val -> goLineConst (ky :. 0) val acc
        | otherwise = acc

    goColumn1 iy ix
        | kh > 0 && kw > 0 =
            case borderInterpolate interpol ih iy of
                Left  iy' -> let !linearIY = iy * iw
                                 !acc = goLine linearIY (Z :. 0 :. 1) (ix + 1)
                                               (img `linearIndex` linearIY)
                             in goColumn (Z :. (iy + 1) :. ix) linearIY (ky + 1) acc'
                Right val -> goLineConst (ky :. 0) val acc
        | otherwise = error "Using FilterFold1 with an empty kernel."

    goLineConst !val !kix@(ky :. kx) !acc
        | kx < kw   = let !acc' = kernel kix val acc
                      in goLineConst val (ky :. (kx + 1)) acc'
        | otherwise = acc

    goLine !linearIY !kix@(ky :. kx) !ix !acc
        | kix < kw  =
            let !val = case borderInterpolate interpol iw ix of
                            Left  ix' -> img `linearIndex` (linearIY + ix)
                            Right val -> val
                !acc' = kernel kix pix acc
            in goLine (kyix :. (kxix + 1)) (iix + 1) acc'
        | otherwise = acc

    !(Z :. cy :. cx) = case anchor of KernelAnchor c     -> c
                                      KernelAnchorCenter -> Z :. (kh `quot` 2)
                                                              :. (kw `quot` 2)

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