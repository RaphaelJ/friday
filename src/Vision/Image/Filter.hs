{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, GADTs
           , MultiParamTypeClasses, TypeFamilies #-}

-- | Provides high level functions to define and apply filters on images.
-- Filters are operations on images on which the surrounding of each processed
-- pixel is considered according to a kernel.
-- See <http://en.wikipedia.org/wiki/Kernel_(image_processing)> for details.
--
-- The @radius@ argument of some filter is used to determine the kernel size.
-- A radius as of 1 means a kernel of size 3, 2 a kernel of size 5 and so on.
module Vision.Image.Filter (
    -- * Types
      Filterable (..), Filter (..)
    , BoxFilter, BoxFilter1, SeparableFilter, SeparableFilter1
    , KernelAnchor (..)
    , Kernel (..)
    , SeparableKernel (..), SeparatelyFiltrable (..)
    , FilterFold (..), FilterFold1 (..)
    , BorderInterpolate (..)
    -- * Functions
    , kernelAnchor, borderInterpolate
    -- * Morphological operators
    , dilate, erode
    -- * Blur
    , blur, gaussianBlur
    -- * Derivation
    , Derivative (..), scharr, sobel
    ) where

import Data.List
import Data.Ratio
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable (Storable)

import Vision.Image.Type (
      MaskedImage (..), Image (..), FromFunction (..)
    , Manifest, Delayed
    )
import Vision.Primitive (Z (..), (:.) (..), DIM1, DIM2, Size, ix1, ix2)

-- Types -----------------------------------------------------------------------

-- | Provides an implementation to execute a type of filter.
-- 'src' is the original image, 'res' the resulting image and 'f' the filter.
class Filterable src res f where
    -- | Applies the given filter on the given image.
    apply :: src -> f -> res

data Filter src kernel init acc res = Filter {
      fKernelSize   :: !Size
    , fKernelCenter :: !KernelAnchor
    , fKernel       :: !kernel
    -- | Defines how the accumulated value is initialized.
    , fInit         :: !init
    , fPost         :: !(src -> acc -> res)
    , fInterpol     :: !(BorderInterpolate src)
    }

-- | 2D filters which are initialized with a value.
type BoxFilter src acc res       = Filter src (Kernel src acc) (FilterFold acc)
                                          acc res

-- | 2D filters which are not initialized with a value.
type BoxFilter1 src res          = Filter src (Kernel src src) FilterFold1 src
                                          res

-- | Separable 2D filters which are initialized with a value.
type SeparableFilter src acc res = Filter src (SeparableKernel src acc)
                                          (FilterFold acc) acc res

-- | Separable 2D filters which are not initialized with a value.
type SeparableFilter1 src res    = Filter src (SeparableKernel src src)
                                          FilterFold1 src res

-- | Defines how to center the kernel will be found.
data KernelAnchor = KernelAnchor !DIM2 | KernelAnchorCenter

-- | A simple 2D kernel.
-- The kernel function accepts the coordinates in the kernel, the value of the
-- pixel at these coordinates ('src'), the current accumulated value and returns
-- a new accumulated value.
-- Non-separable filters computational complexity grows quadratically according
-- to the size of the sides of the kernel.
newtype Kernel src acc = Kernel (DIM2 -> src -> acc -> acc)

-- | Some kernels can be factorized in two uni-dimensional kernels (horizontal
-- and vertical).
-- Separable filters computational complexity grows linearly according to the
-- size of the sides of the kernel.
-- See <http://http://en.wikipedia.org/wiki/Separable_filter>.
data SeparableKernel src acc = SeparableKernel {
    -- | Vertical (column) kernel.
      skVertical   :: !(DIM1 -> src -> acc -> acc)
    -- | Horizontal (row) kernel.
    , skHorizontal :: !(DIM1 -> acc -> acc -> acc)
    }

-- | Used to determine the type of the accumulator image used when computing
-- separable filters. 'src' and 'res' are respectively the source and the result
-- image types while 'acc' is the pixel type of the accumulator.
class SeparatelyFiltrable src res acc where
    type SeparableFilterAccumulator src res acc

instance SeparatelyFiltrable src (Manifest p) acc where
    type SeparableFilterAccumulator src (Manifest p) acc = Manifest acc

instance SeparatelyFiltrable src (Delayed p) acc where
    type SeparableFilterAccumulator src (Delayed p) acc = Delayed acc

-- | Uses an initial value to initialize the filter.
data FilterFold acc = FilterFold acc

-- | Uses the first pixel in the kernel as initial value. The kernel must not be
-- empty and the accumulator type must be the same as the source pixel type.
-- This kind of initialization is needed by morphological filters.
data FilterFold1 = FilterFold1

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

-- Instances -------------------------------------------------------------------

-- Following implementations share a lot of similar processing. However, GHC
-- fails to specialise and optimise correctly when goXXX functions are top-level
-- functions, even with static argument transformations.

-- | Box filters initialized with a given value.
instance (Image src, FromFunction res, src_p ~ ImagePixel src
        , res_p ~ FromFunctionPixel res)
        => Filterable src res (BoxFilter src_p acc res_p) where
    apply !img !(Filter ksize anchor (Kernel kernel) ini post interpol) =
        let !(FilterFold acc)  = ini
        in fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
            let !iy0  = iy - kcy
                !ix0  = ix - kcx
                !safe =    iy0 >= 0 && iy0 + kh <= ih
                        && ix0 >= 0 && ix0 + kw <= iw
                !pix  = img `index` pt
            in post pix $! if safe then goColumnSafe (iy0 * iw) ix0 0 acc
                                   else goColumn     iy0        ix0 0 acc
      where
        !size@(Z :. ih :. iw) = shape img

        !(Z :. kh  :. kw)  = ksize
        !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

        goColumn !iy !ix !ky !acc
            | ky < kh   = case borderInterpolate interpol ih iy of
                            Left  iy' -> goLine iy (iy' * iw) ix ix ky 0 acc
                            Right val -> goLineConst iy ix ky 0 val acc
            | otherwise = acc

        goColumnSafe !linearIY !ix !ky !acc
            | ky < kh   = goLineSafe linearIY ix ix ky 0 acc
            | otherwise = acc

        goLine !iy !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = case borderInterpolate interpol iw ix of
                                Left  ix'  -> img `linearIndex` (linearIY + ix')
                                Right val' -> val'
                    !acc' = kernel (ix2 ky kx) val acc
                in goLine iy linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumn (iy + 1) ix0 (ky + 1) acc

        goLineSafe !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = img `linearIndex` (linearIY + ix)
                    !acc' = kernel (ix2 ky kx) val acc
                in goLineSafe linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumnSafe (linearIY + iw) ix0 (ky + 1) acc

        goLineConst !iy !ix !ky !kx !val !acc
            | kx < kw   = let !acc' = kernel (ix2 ky kx) val acc
                          in goLineConst iy ix ky (kx + 1) val acc'
            | otherwise = goColumn (iy + 1) ix (ky + 1) acc
    {-# INLINE apply #-}

-- | Box filters initialized using the first pixel of the kernel.
instance (Image src, FromFunction res, src_p ~ ImagePixel src
        , res_p ~ FromFunctionPixel res)
        => Filterable src res (BoxFilter1 src_p res_p) where
    apply !img !(Filter ksize anchor (Kernel kernel) _ post interpol)
        | kh == 0 || kw == 0 =
            error "Using FilterFold1 with an empty kernel."
        | otherwise          =
            fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                let !iy0  = iy - kcy
                    !ix0  = ix - kcx
                    !safe =    iy0 >= 0 && iy0 + kh <= ih
                            && ix0 >= 0 && ix0 + kw <= iw
                    !pix  = img `index` pt
                in post pix $! if safe then goColumn1Safe iy0 ix0
                                       else goColumn1     iy0 ix0
      where
        !size@(Z :. ih :. iw) = shape img

        !(Z :. kh  :. kw)  = ksize
        !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

        goColumn1 !iy !ix =
            case borderInterpolate interpol ih iy of
                Left  iy' ->
                    let !linearIY = iy' * iw
                        !acc      = safeIndex linearIY ix
                    in goLine iy linearIY ix (ix + 1) 0 1 acc
                Right val -> goLineConst iy ix 0 1 val val

        goColumn1Safe !iy !ix =
            let !linearIY = iy * iw
                !acc      = img `linearIndex` (linearIY + ix)
            in goLineSafe linearIY ix (ix + 1) 0 1 acc

        goColumn !iy !ix !ky !acc
            | ky < kh   = case borderInterpolate interpol ih iy of
                            Left  iy' -> goLine iy (iy' * iw) ix ix ky 0 acc
                            Right val -> goLineConst iy ix ky 0 val acc
            | otherwise = acc

        goColumnSafe !linearIY !ix !ky !acc
            | ky < kh   = goLineSafe linearIY ix ix ky 0 acc
            | otherwise = acc

        goLine !iy !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = safeIndex linearIY ix
                    !acc' = kernel (ix2 ky kx) val acc
                in goLine iy linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumn (iy + 1) ix0 (ky + 1) acc

        goLineSafe !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = img `linearIndex` (linearIY + ix)
                    !acc' = kernel (ix2 ky kx) val acc
                in goLineSafe linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumnSafe (linearIY + iw) ix0 (ky + 1) acc

        goLineConst !iy !ix !ky !kx !val !acc
            | kx < kw   = let !acc' = kernel (ix2 ky kx) val acc
                          in goLineConst iy ix ky (kx + 1) val acc'
            | otherwise = goColumn (iy + 1) ix (ky + 1) acc

        safeIndex !linearIY !ix =
            case borderInterpolate interpol iw ix of
                Left  ix' -> img `linearIndex` (linearIY + ix')
                Right val -> val
    {-# INLINE apply #-}

-- | Separable filters initialized with a given value.
instance (Image src, FromFunction res, SeparatelyFiltrable src res acc
        , src_p ~ ImagePixel src, res_p ~ FromFunctionPixel res
        , FromFunction      (SeparableFilterAccumulator src res acc)
        , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
        , Image             (SeparableFilterAccumulator src res acc)
        , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
        => Filterable src res (SeparableFilter src_p acc res_p)
            where
    apply !img !f =
        fst $! wrapper img f
      where
        wrapper :: (Image src, FromFunction res
            , FromFunction (SeparableFilterAccumulator src res acc)
            , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
            , Image             (SeparableFilterAccumulator src res acc)
            , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
            => src
            -> SeparableFilter (ImagePixel src) acc (FromFunctionPixel res)
            -> (res, SeparableFilterAccumulator src res acc)
        wrapper !src !(Filter ksize anchor kernel ini post interpol) =
            (res, tmp)
          where
            !size@(Z :. ih :. iw) = shape src

            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

            !(SeparableKernel vert horiz) = kernel
            !(FilterFold acc0)            = ini

            !tmp = fromFunction size $ \(!(Z :. iy :. ix)) ->
                        let !iy0 = iy - kcy
                        in if iy0 >= 0 && iy0 + kh <= ih
                              then goColumnSafe iy0 ix 0 acc0
                              else goColumn     iy0 ix 0 acc0

            !res = fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                        let !ix0 = ix - kcx
                            !pix = src `index` pt
                        in post pix $! if ix0 >= 0 && ix0 + kw <= iw
                                            then goLineSafe (iy * iw) ix0 0 acc0
                                            else goLine     (iy * iw) ix0 0 acc0

            goColumn !iy !ix !ky !acc
                | ky < kh   =
                    let !val  = case borderInterpolate interpol ih iy of
                                    Left  iy'  -> src `index` ix2 iy' ix
                                    Right val' -> val'
                        !acc' = vert (ix1 ky) val acc
                    in goColumn (iy + 1) ix (ky + 1) acc'
                | otherwise = acc

            goColumnSafe !iy !ix !ky !acc
                | ky < kh   =
                    let !val  = src `index` ix2 iy ix
                        !acc' = vert (ix1 ky) val acc
                    in goColumnSafe (iy + 1) ix (ky + 1) acc'
                | otherwise = acc

            goLine !linearIY !ix !kx !acc
                | kx < kw   =
                    let !val =
                            case borderInterpolate interpol iw ix of
                                Left  ix'-> tmp `linearIndex` (linearIY + ix')
                                Right _  -> constLine
                        !acc' = horiz (ix1 kx) val acc
                    in goLine linearIY (ix + 1) (kx + 1) acc'
                | otherwise = acc

            goLineSafe !linearIY !ix !kx !acc
                | kx < kw   =
                    let !val = tmp `linearIndex` (linearIY + ix)
                        !acc' = horiz (ix1 kx) val acc
                    in goLineSafe linearIY (ix + 1) (kx + 1) acc'
                | otherwise = acc

            constLine | BorderConstant val <- interpol =
                        foldl' (\acc ky -> vert (ix1 ky) val acc) acc0 [0..kh-1]
                      | otherwise                      = undefined
        {-# INLINE wrapper #-}
    {-# INLINE apply #-}

-- | Separable filters initialized using the first pixel of the kernel.
instance (Image src, FromFunction res, SeparatelyFiltrable src res src_p
        , src_p ~ ImagePixel src, res_p ~ FromFunctionPixel res
        , FromFunction      (SeparableFilterAccumulator src res src_p)
        , FromFunctionPixel (SeparableFilterAccumulator src res src_p) ~ src_p
        , Image             (SeparableFilterAccumulator src res src_p)
        , ImagePixel        (SeparableFilterAccumulator src res src_p) ~ src_p)
        => Filterable src res (SeparableFilter1 src_p res_p)
            where
    apply !img !f =
        fst $! wrapper img f
      where
        wrapper :: (Image src, FromFunction res, acc ~ ImagePixel src
            , FromFunction (SeparableFilterAccumulator src res acc)
            , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
            , Image             (SeparableFilterAccumulator src res acc)
            , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
            => src
            -> SeparableFilter1 (ImagePixel src) (FromFunctionPixel res)
            -> (res, SeparableFilterAccumulator src res acc)
        wrapper !src !(Filter ksize anchor kernel _ post interpol)
            | kh == 0 || kw == 0 =
                error "Using FilterFold1 with an empty kernel."
            | otherwise          =
                (res, tmp)
          where
            !size@(Z :. ih :. iw) = shape src

            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

            !(SeparableKernel vert horiz) = kernel

            !tmp = fromFunction size $ \(!(Z :. iy :. ix)) ->
                        let !iy0 = iy - kcy
                        in if iy0 >= 0 && iy0 + kh <= ih
                              then goColumn1Safe iy0 ix
                              else goColumn1     iy0 ix

            !res = fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                        let !ix0 = ix - kcx
                            !pix = src `index` pt
                        in post pix $! if ix0 >= 0 && ix0 + kw <= iw
                                            then goLine1Safe (iy * iw) ix0
                                            else goLine1     (iy * iw) ix0

            goColumn1 !iy !ix =
                case borderInterpolate interpol ih iy of
                    Left  iy' ->
                        let !acc = src `index` ix2 iy' ix
                        in goColumn (iy + 1) ix 1 acc
                    Right val ->
                        goColumn (iy + 1) ix 1 val

            goColumn1Safe !iy !ix =
                let !linearIY = iy * iw
                    !acc      = src `linearIndex` (linearIY + ix)
                in goColumnSafe (linearIY + iw) ix 1 acc

            goColumn !iy !ix !ky !acc
                | ky < kh   =
                    let !val  = case borderInterpolate interpol ih iy of
                                    Left  iy'  -> src `index` ix2 iy' ix
                                    Right val' -> val'
                        !acc' = vert (ix1 ky) val acc
                    in goColumn (iy + 1) ix (ky + 1) acc'
                | otherwise = acc

            goColumnSafe !linearIY !ix !ky !acc
                | ky < kh   =
                    let !val  = src `linearIndex` (linearIY + ix)
                        !acc' = vert (ix1 ky) val acc
                    in goColumnSafe (linearIY + iw) ix (ky + 1) acc'
                | otherwise = acc

            goLine1 !linearIY !ix =
                let !acc =
                        case borderInterpolate interpol iw ix of
                            Left  ix' -> tmp `linearIndex` (linearIY + ix')
                            Right _   -> columnConst
                in goLine linearIY (ix + 1) 1 acc

            goLine1Safe !linearIY !ix =
                let !linearIX = linearIY + ix
                    !acc      = tmp `linearIndex` linearIX
                in goLineSafe (linearIX + 1) 1 acc

            goLine !linearIY !ix !kx !acc
                | kx < kw   =
                    let !val =
                            case borderInterpolate interpol iw ix of
                                Left  ix'-> tmp `linearIndex` (linearIY + ix')
                                Right _  -> columnConst
                        !acc' = horiz (ix1 kx) val acc
                    in goLine linearIY (ix + 1) (kx + 1) acc'
                | otherwise = acc

            goLineSafe !linearIX !kx !acc
                | kx < kw   =
                    let !val = tmp `linearIndex` linearIX
                        !acc' = horiz (ix1 kx) val acc
                    in goLineSafe (linearIX + 1) (kx + 1) acc'
                | otherwise = acc

            columnConst
                | BorderConstant val <- interpol = goColumnConst 1 val val
                | otherwise                      = undefined

            goColumnConst !ky !val !acc
                | ky < kh   = goColumnConst (ky + 1) val (vert (ix1 ky) acc val)
                | otherwise = acc
        {-# INLINE wrapper #-}
    {-# INLINE apply #-}

-- Functions -------------------------------------------------------------------

-- | Given a method to compute the kernel anchor and the size of the kernel,
-- returns the anchor of the kernel as coordinates.
kernelAnchor :: KernelAnchor -> Size -> DIM2
kernelAnchor (KernelAnchor ix)    _               = ix
kernelAnchor (KernelAnchorCenter) (Z :. kh :. kw) = ix2 (round $ (kh - 1) % 2)
                                                        (round $ (kw - 1) % 2)

-- | Given a method of interpolation, the number of pixel in the dimension and
-- an index in this dimension, returns either the index of the interpolated
-- pixel or a constant value.
borderInterpolate :: BorderInterpolate a
                  -> Int -- ^ The size of the dimension.
                  -> Int -- ^ The index in the dimension.
                  -> Either Int a
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

-- Morphological operators -----------------------------------------------------

dilate :: Ord src => Int -> SeparableFilter1 src src
dilate radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel kernel kernel)
           FilterFold1 (\_ acc -> acc) BorderReplicate
  where
    !size = radius * 2 + 1

    kernel _ = max
{-# INLINE dilate #-}

erode :: Ord src => Int -> SeparableFilter1 src src
erode radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel kernel kernel)
           FilterFold1 (\_ acc -> acc) BorderReplicate
  where
    !size = radius * 2 + 1

    kernel _ = min
{-# INLINE erode #-}

-- Blur ------------------------------------------------------------------------

-- Blur the image by averaging the pixel inside the kernel.
-- Considers using a type for 'acc' with
-- @maxBound acc >= maxBound src * (kernel size)²@.
blur :: (Integral src, Integral acc, Num res)
     => Int -- ^ Blur radius.
     -> SeparableFilter src acc res
blur radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz)
           (FilterFold 0) post BorderReplicate
  where
    !size  = radius * 2 + 1
    !nPixs = fromIntegral $ square size

    vert  _ !val  !acc = acc + fromIntegral val

    horiz _ !acc' !acc = acc + acc'

    post _ acc = fromIntegral $ acc `div` nPixs
{-# INLINE blur #-}

-- Blur the image by averaging the pixel inside the kernel using a Gaussian
-- function.
-- See <http://en.wikipedia.org/wiki/Gaussian_blur>
gaussianBlur :: (Integral src, Floating acc, RealFrac acc, Storable acc
               , Integral res)
             => Int         -- ^ Blur radius.
             -- | Sigma value of the Gaussian function. If not given, will be
             -- automatically computed from the radius so that the kernel
             -- fits 3σ of the distribution.
             -> Maybe acc
             -> SeparableFilter src acc res
gaussianBlur !radius !mSig =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz)
           (FilterFold 0) (\_ !acc -> round $ acc / kernelSum) BorderReplicate
  where
    !size = radius * 2 + 1

    -- If σ is not provided, tries to fit 3σ in the kernel.
    !sig = case mSig of Just s  -> s
                        Nothing -> (0.5 + fromIntegral radius) / 3

    vert  !(Z :. y) !val !acc = let !coeff = kernelVec V.! y
                                in acc + fromIntegral val * coeff

    horiz !(Z :. x) !val !acc = let !coeff = kernelVec V.! x
                                in acc + val * coeff

    !kernelVec =
        V.generate size $ \x ->
            gaussian $! fromIntegral $! abs $! x - radius

    !kernelSum = V.sum kernelVec

    gaussian !x = invSigSqrt2Pi * exp (inv2xSig2 * square x)

    -- Pre-computed terms of the Gaussian function.
    !invSigSqrt2Pi = 1 / (sig * sqrt (2 * pi))
    !inv2xSig2     = -1 / (2 * square sig)
{-# INLINE gaussianBlur #-}

-- Derivation ------------------------------------------------------------------

data Derivative = DerivativeX | DerivativeY

-- | Estimates the first derivative using the Scharr's 3x3 kernel.
-- Considers using a signed integer type for 'res' with
-- @maxBound res >= 16 * maxBound src@.
scharr :: (Integral src, Integral res)
       => Derivative -> SeparableFilter src res res
scharr der =
    let !kernel =
            case der of
                DerivativeX -> SeparableKernel kernel1 kernel2
                DerivativeY -> SeparableKernel kernel2 kernel1
    in Filter (ix2 3 3) KernelAnchorCenter kernel (FilterFold 0) (\_ acc -> acc)
              BorderReplicate
  where
    kernel1 !(Z :. 1)  !val !acc = acc + 10 * fromIntegral val
    kernel1 !(Z :. _)  !val !acc = acc + 3  * fromIntegral val

    kernel2 !(Z :.  0) !val !acc = acc - fromIntegral val
    kernel2 !(Z :.  1) !_   !acc = acc
    kernel2 !(Z :. ~2) !val !acc = acc + fromIntegral val
{-# INLINE scharr #-}

-- | Estimates the first derivative using a Sobel's kernel.
-- Prefer 'scharr' when radius equals @1@ as Scharr's kernel is more precise and
-- implemented faster.
-- Considers using a signed integer type for 'res' which is significantly larger
-- than 'src', especially for large kernels.
sobel :: (Integral src, Integral res, Storable res)
      => Int        -- ^ Kernel radius.
      -> Derivative
      -> SeparableFilter src res res
sobel radius der =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz)
           (FilterFold 0) (\_ acc -> acc) BorderReplicate
  where
    !size = radius * 2 + 1

    vert  !(Z :. x) !val !acc = let !coeff = vec1 V.! x
                                in acc + fromIntegral val * coeff

    horiz !(Z :. x) !val !acc = let !coeff = vec2 V.! x
                                in acc + fromIntegral val * coeff

    !radius' = fromIntegral radius

    (!vec1, !vec2) = case der of DerivativeX -> (vec1', vec2')
                                 DerivativeY -> (vec2', vec1')

    !vec1' = let pows = [ 2^i | i <- [0..radius'] ]
             in V.fromList $ pows ++ (tail (reverse pows))
    !vec2' = V.fromList $ [1..radius'] ++ [0] ++ map negate [1..radius']
{-# INLINE sobel #-}

square :: Num a => a -> a
square a = a * a

word :: Integral a => a -> Word
word = fromIntegral
