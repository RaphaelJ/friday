{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, GADTs
           , MultiParamTypeClasses, TypeFamilies, TupleSections
           , ScopedTypeVariables #-}

-- | Provides high level functions to define and apply filters on images.
--
-- Filters are operations on images on which the surrounding of each processed
-- pixel is considered according to a kernel.
--
-- See <http://en.wikipedia.org/wiki/Kernel_(image_processing)> for details.
--
-- The @radius@ argument of some filters is used to determine the kernel size.
-- A radius as of 1 means a kernel of size 3, 2 a kernel of size 5 and so on.
--
-- The @acc@ type argument of some filters defines the type which will be used
-- to store the accumulated value of the kernel (e.g. by setting @acc@ to
-- 'Double' in the computation of a Gaussian blur, the kernel average will be
-- computed using a 'Double').
--
-- To apply a filter to an image, use the 'apply' method:
--
-- @
-- let -- Creates a filter which will blur the image. Uses a 'Double' as
--     -- accumulator of the Gaussian kernel.
--     filter :: 'Blur' GreyPixel Double GreyPixel
--     filter = 'gaussianBlur' 2 Nothing
-- in 'apply' filter img :: Grey
-- @
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
    -- * Filters
    -- ** Morphological operators
    , Morphological, dilate, erode
    -- ** Blur
    , Blur, blur, gaussianBlur
    -- ** Derivation
    , Derivative, DerivativeType (..), scharr, sobel
    -- ** Others
    , Mean, mean
    ) where

import Data.List
import Data.Ratio
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable (Storable)

import Vision.Image.Type (
      MaskedImage (..), Image (..), FromFunction (..)
    , Manifest, Delayed
    , (!)
    )
import Vision.Primitive (Z (..), (:.) (..), DIM1, DIM2, Size, ix1, ix2)

-- Types -----------------------------------------------------------------------

-- | Provides an implementation to execute a type of filter.
--
-- 'src' is the original image, 'res' the resulting image and 'f' the filter.
class Filterable src res f where
    -- | Applies the given filter on the given image.
    apply :: f -> src -> res

data Filter src kernel init fold acc res = Filter {
      fKernelSize   :: !Size
    , fKernelCenter :: !KernelAnchor
    -- | See 'Kernel' and 'SeparableKernel'.
    , fKernel       :: !kernel
    -- | Computes a constant value for each pixel before applying the kernel.
    --
    -- This value will be passed to 'fKernel' functions and to the 'fPost'
    -- function.
    -- For most filters, @fInit@ will be @\_ _ -> ()@.
    , fInit         :: !(DIM2 -> src -> init)
    -- | Defines how the accumulated value is initialized.
    --
    -- See 'FilterFold' and 'FilterFold1'.
    , fFold         :: !fold
    -- | This function will be executed after the iteration of the kernel on a
    -- given point.
    --
    -- Can be used to normalize the accumulated value, for example.
    , fPost         :: !(DIM2 -> src -> init -> acc -> res)
    , fInterpol     :: !(BorderInterpolate src)
    }

-- | 2D filters which are initialized with a value.
type BoxFilter src init acc res       = Filter src (Kernel src init acc) init
                                               (FilterFold acc) acc res

-- | 2D filters which are not initialized with a value.
type BoxFilter1 src init res          = Filter src (Kernel src init src) init
                                               FilterFold1 src res

-- | Separable 2D filters which are initialized with a value.
type SeparableFilter src init acc res = Filter src
                                               (SeparableKernel src init acc)
                                               init (FilterFold acc) acc res

-- | Separable 2D filters which are not initialized with a value.
type SeparableFilter1 src init res    = Filter src
                                               (SeparableKernel src init src)
                                               init FilterFold1 src res

-- | Defines how the center of the kernel will be determined.
data KernelAnchor = KernelAnchor !DIM2 | KernelAnchorCenter

-- | A simple 2D kernel.
--
-- The kernel function accepts the coordinates in the kernel, the value of the
-- pixel at these coordinates ('src'), the current accumulated value and returns
-- a new accumulated value.
--
-- Non-separable filters computational complexity grows quadratically according
-- to the size of the sides of the kernel.
newtype Kernel src init acc = Kernel (init -> DIM2 -> src -> acc -> acc)

-- | Some kernels can be factorized in two uni-dimensional kernels (horizontal
-- and vertical).
--
-- Separable filters computational complexity grows linearly according to the
-- size of the sides of the kernel.
--
-- See <http://http://en.wikipedia.org/wiki/Separable_filter>.
data SeparableKernel src init acc = SeparableKernel {
    -- | Vertical (column) kernel.
      skVertical   :: !(init -> DIM1 -> src -> acc -> acc)
    -- | Horizontal (row) kernel.
    , skHorizontal :: !(init -> DIM1 -> acc -> acc -> acc)
    }

-- | Used to determine the type of the accumulator image used when computing
-- separable filters.
--
-- 'src' and 'res' are respectively the source and the result image types while
-- 'acc' is the pixel type of the accumulator.
class SeparatelyFiltrable src res acc where
    type SeparableFilterAccumulator src res acc

instance SeparatelyFiltrable src (Manifest p) acc where
    type SeparableFilterAccumulator src (Manifest p) acc = Manifest acc

instance SeparatelyFiltrable src (Delayed p) acc where
    type SeparableFilterAccumulator src (Delayed p) acc = Delayed acc

-- | Uses the result of the provided function as the initial value of the
-- kernel's accumulator, depending on the center coordinates in the image.
--
-- For most filters, the function will always return the same value (i.e.
-- defined as @const 0@), but this kind of initialization could be required for
-- some filters.
data FilterFold acc = FilterFold (DIM2 -> acc)

-- | Uses the first pixel in the kernel as initial value. The kernel must not be
-- empty and the accumulator type must be the same as the source pixel type.
--
-- This kind of initialization is needed by morphological filters.
data FilterFold1 = FilterFold1

-- | Defines how image boundaries are extrapolated by the algorithms.
--
-- '|' characters in examples are image borders.
data BorderInterpolate a =
    -- | Replicates the first and last pixels of the image.
    --
    -- > aaaaaa|abcdefgh|hhhhhhh
      BorderReplicate
    -- | Reflects the border of the image.
    --
    -- > fedcba|abcdefgh|hgfedcb
    | BorderReflect
    -- | Considers that the last pixel of the image is before the first one.
    --
    -- > cdefgh|abcdefgh|abcdefg
    | BorderWrap
    -- | Assigns a constant value to out of image pixels.
    --
    -- > iiiiii|abcdefgh|iiiiiii  with some specified 'i'
    | BorderConstant !a

-- Instances -------------------------------------------------------------------

-- Following implementations share a lot of similar processing. However, GHC
-- fails to specialise and optimise correctly when goXXX functions are top-level
-- functions, even with static argument transformations.

-- | Box filters initialized with a given value.
instance (Image src, FromFunction res, src_p ~ ImagePixel src
        , res_p ~ FromFunctionPixel res)
        => Filterable src res (BoxFilter src_p init acc res_p) where
    apply !(Filter ksize anchor (Kernel kernel) initF fold post interpol) !img =
        let !(FilterFold fAcc)  = fold
        in fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
            let pix   = img ! pt
                !ini  = initF pt pix
                !acc  = fAcc pt
                !iy0  = iy - kcy
                !ix0  = ix - kcx
                !safe =    iy0 >= 0 && iy0 + kh <= ih
                        && ix0 >= 0 && ix0 + kw <= iw
            in post pt pix ini $!
                    if safe then goColumnSafe ini (iy0 * iw) ix0 0 acc
                            else goColumn     ini iy0        ix0 0 acc
      where
        !size@(Z :. ih :. iw) = shape img

        !(Z :. kh  :. kw)  = ksize
        !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

        goColumn !ini !iy !ix !ky !acc
            | ky < kh   = case borderInterpolate interpol ih iy of
                            Left  iy' -> goLine ini iy (iy' * iw) ix ix ky 0 acc
                            Right val -> goLineConst ini iy ix ky 0 val acc
            | otherwise = acc

        goColumnSafe !ini !linearIY !ix !ky !acc
            | ky < kh   = goLineSafe ini linearIY ix ix ky 0 acc
            | otherwise = acc

        goLine !ini !iy !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = case borderInterpolate interpol iw ix of
                                Left  ix'  -> img `linearIndex` (linearIY + ix')
                                Right val' -> val'
                    !acc' = kernel ini (ix2 ky kx) val acc
                in goLine ini iy linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumn ini (iy + 1) ix0 (ky + 1) acc

        goLineSafe !ini !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = img `linearIndex` (linearIY + ix)
                    !acc' = kernel ini (ix2 ky kx) val acc
                in goLineSafe ini linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumnSafe ini (linearIY + iw) ix0 (ky + 1) acc

        goLineConst !ini !iy !ix !ky !kx !val !acc
            | kx < kw   = let !acc' = kernel ini (ix2 ky kx) val acc
                          in goLineConst ini iy ix ky (kx + 1) val acc'
            | otherwise = goColumn ini (iy + 1) ix (ky + 1) acc
    {-# INLINE apply #-}

-- | Box filters initialized using the first pixel of the kernel.
instance (Image src, FromFunction res, src_p ~ ImagePixel src
        , res_p ~ FromFunctionPixel res)
        => Filterable src res (BoxFilter1 src_p init res_p) where
    apply !(Filter ksize anchor (Kernel kernel) initF _ post interpol) !img
        | kh == 0 || kw == 0 =
            error "Using FilterFold1 with an empty kernel."
        | otherwise          =
            fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                let pix   = img ! pt
                    !ini  = initF pt pix
                    !iy0  = iy - kcy
                    !ix0  = ix - kcx
                    !safe =    iy0 >= 0 && iy0 + kh <= ih
                            && ix0 >= 0 && ix0 + kw <= iw
                in post pt pix ini $! if safe then goColumn1Safe ini iy0 ix0
                                              else goColumn1     ini iy0 ix0
      where
        !size@(Z :. ih :. iw) = shape img

        !(Z :. kh  :. kw)  = ksize
        !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

        goColumn1 !ini !iy !ix =
            case borderInterpolate interpol ih iy of
                Left  iy' ->
                    let !linearIY = iy' * iw
                        !acc      = safeIndex linearIY ix
                    in goLine ini iy linearIY ix (ix + 1) 0 1 acc
                Right val -> goLineConst ini iy ix 0 1 val val

        goColumn1Safe !ini !iy !ix =
            let !linearIY = iy * iw
                !acc      = img `linearIndex` (linearIY + ix)
            in goLineSafe ini linearIY ix (ix + 1) 0 1 acc

        goColumn !ini !iy !ix !ky !acc
            | ky < kh   = case borderInterpolate interpol ih iy of
                            Left  iy' -> goLine ini iy (iy' * iw) ix ix ky 0 acc
                            Right val -> goLineConst ini iy ix ky 0 val acc
            | otherwise = acc

        goColumnSafe !ini !linearIY !ix !ky !acc
            | ky < kh   = goLineSafe ini linearIY ix ix ky 0 acc
            | otherwise = acc

        goLine !ini !iy !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = safeIndex linearIY ix
                    !acc' = kernel ini (ix2 ky kx) val acc
                in goLine ini iy linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumn ini (iy + 1) ix0 (ky + 1) acc

        goLineSafe !ini !linearIY !ix0 !ix !ky !kx !acc
            | kx < kw   =
                let !val  = img `linearIndex` (linearIY + ix)
                    !acc' = kernel ini (ix2 ky kx) val acc
                in goLineSafe ini linearIY ix0 (ix + 1) ky (kx + 1) acc'
            | otherwise = goColumnSafe ini (linearIY + iw) ix0 (ky + 1) acc

        goLineConst !ini !iy !ix !ky !kx !val !acc
            | kx < kw   = let !acc' = kernel ini (ix2 ky kx) val acc
                          in goLineConst ini iy ix ky (kx + 1) val acc'
            | otherwise = goColumn ini (iy + 1) ix (ky + 1) acc

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
        => Filterable src res (SeparableFilter src_p init acc res_p)
            where
    apply !f !img =
        fst $! wrapper img f
      where
        wrapper :: (Image src, FromFunction res
            , FromFunction (SeparableFilterAccumulator src res acc)
            , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
            , Image             (SeparableFilterAccumulator src res acc)
            , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
            => src
            -> SeparableFilter (ImagePixel src) init acc (FromFunctionPixel res)
            -> (res, SeparableFilterAccumulator src res acc)
        wrapper !src !(Filter ksize anchor kernel initF fold post interpol) =
            (res, tmp)
          where
            !size@(Z :. ih :. iw) = shape src

            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

            !(SeparableKernel vert horiz) = kernel
            !(FilterFold fAcc)            = fold

            !tmp = fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                        let pix   = src ! pt
                            !ini  = initF pt pix
                            !acc0 = fAcc pt
                            !iy0  = iy - kcy
                        in if iy0 >= 0 && iy0 + kh <= ih
                              then goColumnSafe ini iy0 ix 0 acc0
                              else goColumn     ini iy0 ix 0 acc0

            !res = fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                        let pix   = src ! pt
                            !ini  = initF pt pix
                            !acc0 = fAcc pt
                            !ix0  = ix - kcx
                        in post pt pix ini $!
                                if ix0 >= 0 && ix0 + kw <= iw
                                    then goLineSafe ini (iy * iw) ix0 0 acc0
                                    else goLine     ini acc0 (iy * iw) ix0 0
                                                    acc0

            goColumn !ini !iy !ix !ky !acc
                | ky < kh   =
                    let !val  = case borderInterpolate interpol ih iy of
                                    Left  iy'  -> src ! ix2 iy' ix
                                    Right val' -> val'
                        !acc' = vert ini (ix1 ky) val acc
                    in goColumn ini (iy + 1) ix (ky + 1) acc'
                | otherwise = acc

            goColumnSafe !ini !iy !ix !ky !acc
                | ky < kh   =
                    let !val  = src ! ix2 iy ix
                        !acc' = vert ini (ix1 ky) val acc
                    in goColumnSafe ini (iy + 1) ix (ky + 1) acc'
                | otherwise = acc

            goLine !ini !acc0 !linearIY !ix !kx !acc
                | kx < kw   =
                    let !val =
                            case borderInterpolate interpol iw ix of
                                Left  ix'  -> tmp `linearIndex` (linearIY + ix')
                                Right val' -> constCol ini acc0 val'
                        !acc' = horiz ini (ix1 kx) val acc
                    in goLine ini acc0 linearIY (ix + 1) (kx + 1) acc'
                | otherwise = acc

            goLineSafe !ini !linearIY !ix !kx !acc
                | kx < kw   =
                    let !val = tmp `linearIndex` (linearIY + ix)
                        !acc' = horiz ini (ix1 kx) val acc
                    in goLineSafe ini linearIY (ix + 1) (kx + 1) acc'
                | otherwise = acc

            -- Computes the value of an out of image column when the
            -- interpolation method is BorderConstant.
            constCol !ini !acc0 !constVal =
                foldl' (\acc ky -> vert ini (ix1 ky) constVal acc) acc0
                       [0..kh-1]
        {-# INLINE wrapper #-}
    {-# INLINE apply #-}

-- | Separable filters initialized using the first pixel of the kernel.
instance (Image src, FromFunction res, SeparatelyFiltrable src res src_p
        , src_p ~ ImagePixel src, res_p ~ FromFunctionPixel res
        , FromFunction      (SeparableFilterAccumulator src res src_p)
        , FromFunctionPixel (SeparableFilterAccumulator src res src_p) ~ src_p
        , Image             (SeparableFilterAccumulator src res src_p)
        , ImagePixel        (SeparableFilterAccumulator src res src_p) ~ src_p)
        => Filterable src res (SeparableFilter1 src_p init res_p)
            where
    apply !f !img =
        fst $! wrapper img f
      where
        wrapper :: (Image src, FromFunction res, acc ~ ImagePixel src
            , FromFunction (SeparableFilterAccumulator src res acc)
            , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
            , Image             (SeparableFilterAccumulator src res acc)
            , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
            => src
            -> SeparableFilter1 (ImagePixel src) init (FromFunctionPixel res)
            -> (res, SeparableFilterAccumulator src res acc)
        wrapper !src !(Filter ksize anchor kernel initF _ post interpol)
            | kh == 0 || kw == 0 =
                error "Using FilterFold1 with an empty kernel."
            | otherwise          =
                (res, tmp)
          where
            !size@(Z :. ih :. iw) = shape src

            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

            !(SeparableKernel vert horiz) = kernel

            !tmp = fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                        let pix  = src ! pt
                            !ini = initF pt pix
                            !iy0 = iy - kcy
                        in if iy0 >= 0 && iy0 + kh <= ih
                              then goColumn1Safe ini iy0 ix
                              else goColumn1     ini iy0 ix

            !res = fromFunction size $ \(!pt@(Z :. iy :. ix)) ->
                        let pix  = src ! pt
                            !ini = initF pt pix
                            !ix0 = ix - kcx
                        in post pt pix ini $!
                                if ix0 >= 0 && ix0 + kw <= iw
                                    then goLine1Safe ini (iy * iw) ix0
                                    else goLine1     ini (iy * iw) ix0

            goColumn1 !ini !iy !ix =
                case borderInterpolate interpol ih iy of
                    Left  iy' ->
                        let !acc = src ! ix2 iy' ix
                        in goColumn ini (iy + 1) ix 1 acc
                    Right val ->
                        goColumn ini (iy + 1) ix 1 val

            goColumn1Safe !ini !iy !ix =
                let !linearIY = iy * iw
                    !acc      = src `linearIndex` (linearIY + ix)
                in goColumnSafe ini (linearIY + iw) ix 1 acc

            goColumn !ini !iy !ix !ky !acc
                | ky < kh   =
                    let !val  = case borderInterpolate interpol ih iy of
                                    Left  iy'  -> src ! ix2 iy' ix
                                    Right val' -> val'
                        !acc' = vert ini (ix1 ky) val acc
                    in goColumn ini (iy + 1) ix (ky + 1) acc'
                | otherwise = acc

            goColumnSafe !ini !linearIY !ix !ky !acc
                | ky < kh   =
                    let !val  = src `linearIndex` (linearIY + ix)
                        !acc' = vert ini (ix1 ky) val acc
                    in goColumnSafe ini (linearIY + iw) ix (ky + 1) acc'
                | otherwise = acc

            goLine1 !ini !linearIY !ix =
                let !acc =
                        case borderInterpolate interpol iw ix of
                            Left  ix' -> tmp `linearIndex` (linearIY + ix')
                            Right val -> columnConst ini val
                in goLine ini linearIY (ix + 1) 1 acc

            goLine1Safe !ini !linearIY !ix =
                let !linearIX = linearIY + ix
                    !acc      = tmp `linearIndex` linearIX
                in goLineSafe ini (linearIX + 1) 1 acc

            goLine !ini !linearIY !ix !kx !acc
                | kx < kw   =
                    let !val =
                            case borderInterpolate interpol iw ix of
                                Left  ix'  -> tmp `linearIndex` (linearIY + ix')
                                Right val' -> columnConst ini val'
                        !acc' = horiz ini (ix1 kx) val acc
                    in goLine ini linearIY (ix + 1) (kx + 1) acc'
                | otherwise = acc

            goLineSafe !ini !linearIX !kx !acc
                | kx < kw   =
                    let !val = tmp `linearIndex` linearIX
                        !acc' = horiz ini (ix1 kx) val acc
                    in goLineSafe ini (linearIX + 1) (kx + 1) acc'
                | otherwise = acc

            columnConst !ini !constVal = goColumnConst ini 1 constVal constVal

            goColumnConst !ini !ky !constVal !acc
                | ky < kh   = goColumnConst ini (ky + 1) constVal
                                            (vert ini (ix1 ky) acc constVal)
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

type Morphological pix = SeparableFilter1 pix () pix

dilate :: Ord pix => Int -> Morphological pix
dilate radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel kernel kernel)
           (\_ _ -> ()) FilterFold1 (\_ _ _ !acc -> acc) BorderReplicate
  where
    !size = radius * 2 + 1

    kernel _ _ = max
{-# INLINE dilate #-}

erode :: Ord pix => Int -> Morphological pix
erode radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel kernel kernel)
           (\_ _ -> ()) FilterFold1 (\_ _ _ !acc -> acc) BorderReplicate
  where
    !size = radius * 2 + 1

    kernel _ _ = min
{-# INLINE erode #-}

-- Blur ------------------------------------------------------------------------

type Blur src acc res = SeparableFilter src () acc res

-- | Blurs the image by averaging the pixel inside the kernel.
--
-- Considers using a type for 'acc' with
-- @maxBound acc >= maxBound src * (kernel size)²@.
blur :: (Integral src, Integral acc, Num res)
     => Int -- ^ Blur radius.
     -> Blur src acc res
blur radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz)
           (\_ _ -> ()) (FilterFold (const 0)) post BorderReplicate
  where
    !size  = radius * 2 + 1
    !nPixs = fromIntegral $ square size

    vert  _ _ !val  !acc = acc + fromIntegral val
    horiz _ _ !acc' !acc = acc + acc'

    post _ _ _ !acc = fromIntegral $ acc `div` nPixs
{-# INLINE blur #-}

-- | Blurs the image by averaging the pixel inside the kernel using a Gaussian
-- function.
--
-- See <http://en.wikipedia.org/wiki/Gaussian_blur>
gaussianBlur :: (Integral src, Floating acc, RealFrac acc, Storable acc
               , Integral res)
             => Int -- ^ Blur radius.
             -> Maybe acc
             -- ^ Sigma value of the Gaussian function. If not given, will be
             -- automatically computed from the radius so that the kernel
             -- fits 3σ of the distribution.
             -> Blur src acc res
gaussianBlur !radius !mSig =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz) 
           (\_ _ -> ()) (FilterFold (const 0)) (\_ _ _ !acc -> round acc)
           BorderReplicate
  where
    !size = radius * 2 + 1

    -- If σ is not provided, tries to fit 3σ in the kernel.
    !sig = case mSig of Just s  -> s
                        Nothing -> (0.5 + fromIntegral radius) / 3

    vert  _ !(Z :. y) !val !acc = let !coeff = kernelVec V.! y
                                  in acc + fromIntegral val * coeff

    horiz _ !(Z :. x) !val !acc = let !coeff = kernelVec V.! x
                                  in acc + val * coeff

    !kernelVec =
        -- Creates a vector of Gaussian values and normalizes it so its sum
        -- equals 1.
        let !unormalized = V.generate size $ \x ->
                                gaussian $! fromIntegral $! abs $! x - radius
            !kernelSum   = V.sum unormalized
        in V.map (/ kernelSum) unormalized

    gaussian !x = invSigSqrt2Pi * exp (inv2xSig2 * square x)

    -- Pre-computed terms of the Gaussian function.
    !invSigSqrt2Pi = 1 / (sig * sqrt (2 * pi))
    !inv2xSig2     = -1 / (2 * square sig)
{-# INLINE gaussianBlur #-}

-- Derivation ------------------------------------------------------------------

type Derivative src res = SeparableFilter src () res res

data DerivativeType = DerivativeX | DerivativeY

-- | Estimates the first derivative using the Scharr's 3x3 kernel.
--
-- Convolves the following kernel for the X derivative:
--
-- @
--  -3   0   3
-- -10   0  10
--  -3   0   3
-- @
--
-- And this kernel for the Y derivative:
--
-- @
--  -3 -10  -3
--   0   0   0
--   3  10   3
-- @
--
-- Considers using a signed integer type for 'res' with
-- @maxBound res >= 16 * maxBound src@.
scharr :: (Integral src, Integral res)
       => DerivativeType -> Derivative src res
scharr der =
    let !kernel =
            case der of
                DerivativeX -> SeparableKernel kernel1 kernel2
                DerivativeY -> SeparableKernel kernel2 kernel1
    in Filter (ix2 3 3) KernelAnchorCenter kernel (\_ _ -> ())
              (FilterFold (const 0)) (\_ _ _ !acc -> acc) BorderReplicate
  where
    kernel1 _ !(Z :. 1)  !val !acc = acc + 10 * fromIntegral val
    kernel1 _ !(Z :. _)  !val !acc = acc + 3  * fromIntegral val

    kernel2 _ !(Z :.  0) !val !acc = acc - fromIntegral val
    kernel2 _ !(Z :.  1) !_   !acc = acc
    kernel2 _ !(Z :. ~2) !val !acc = acc + fromIntegral val
{-# INLINE scharr #-}

-- | Estimates the first derivative using a Sobel's kernel.
--
-- Prefer 'scharr' when radius equals @1@ as Scharr's kernel is more accurate
-- and is implemented faster.
--
-- Considers using a signed integer type for 'res' which is significantly larger
-- than 'src', especially for large kernels.
sobel :: (Integral src, Integral res, Storable res)
      => Int        -- ^ Kernel radius.
      -> DerivativeType
      -> Derivative src res
sobel radius der =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz)
           (\_ _ -> ()) (FilterFold (const 0)) (\_ _ _ !acc -> acc)
           BorderReplicate
  where
    !size = radius * 2 + 1

    vert  _ !(Z :. x) !val !acc = let !coeff = vec1 V.! x
                                  in acc + fromIntegral val * coeff

    horiz _ !(Z :. x) !val !acc = let !coeff = vec2 V.! x
                                  in acc + fromIntegral val * coeff

    !radius' = fromIntegral radius

    (!vec1, !vec2) = case der of DerivativeX -> (vec1', vec2')
                                 DerivativeY -> (vec2', vec1')

    !vec1' = let pows = [ 2^i | i <- [0..radius'] ]
             in V.fromList $ pows ++ (tail (reverse pows))
    !vec2' = V.fromList $ map negate [1..radius'] ++ [0] ++ [1..radius']
{-# INLINE sobel #-}

-- Others ----------------------------------------------------------------------

type Mean src acc res = SeparableFilter src () acc res

-- | Computes the average of a kernel of the given size.
--
-- This is similar to 'blur' but with a rectangular kernel and a 'Fractional'
-- result.
mean :: (Integral src, Integral acc, Fractional res)
     => Size -> SeparableFilter src () acc res
mean size@(Z :. h :. w) =
    Filter size KernelAnchorCenter (SeparableKernel vert horiz) (\_ _ -> ())
           (FilterFold (const 0)) post BorderReplicate
  where
    vert  _ _ !val  !acc = acc + fromIntegral val
    horiz _ _ !acc' !acc = acc + acc'

    !nPixsFactor = 1 / (fromIntegral $! h * w)
    post _ _ _ !acc  = fromIntegral acc * nPixsFactor
{-# INLINE mean #-}

-- -----------------------------------------------------------------------------

square :: Num a => a -> a
square a = a * a

word :: Integral a => a -> Word
word = fromIntegral
