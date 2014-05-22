{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, GADTs
           , MultiParamTypeClasses, TypeFamilies #-}

-- | Provides high level functions to apply filters on images.
module Vision.Image.Filter (
      Filterable (..), Filter (..), BoxFilter, SeparableFilter, Kernel (..)
    , SeparableKernel, SeparatelyFiltrable (..), FilterFold (..), FilterFold1
    , BorderInterpolate (..)
    , kernelAnchor, borderInterpolate
    , blur, gaussianBlur, scharr
    ) where

import Data.List
import Data.Ratio
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable (Storable)

import Vision.Image.Type (
      Pixel (..), MaskedImage (..), Image (..), FromFunction (..)
    , Manifest, Delayed
    )
import Vision.Primitive (Z (..), (:.) (..), DIM1, DIM2, Size, ix1, ix2)

class Filterable src res f where
    -- | Applies the given filter on the given image.
    apply :: src -> f -> res

data Filter src kernel init acc res = Filter {
      fKernelSize   :: !Size
    , fKernelCenter :: !KernelAnchor
    , fKernel       :: !kernel
    -- | Defines how the accumulated value is initialized.
    , fInit         :: !init
    , fPost         :: !(acc -> res)
    , fInterpol     :: !(BorderInterpolate src)
    }

type BoxFilter src init acc res = Filter src (Kernel src acc) init acc res

type SeparableFilter src init acc res = Filter src (SeparableKernel src acc)
                                               init acc res

-- | Defines how to center the kernel will be found.
data KernelAnchor = KernelAnchor !DIM2 | KernelAnchorCenter

-- | A simple 2D kernel.
-- The kernel function accepts the coordinates in the kernel, the value of the
-- pixel at these coordinates ('src'), the current accumulated value and returns
-- a new accumulated value.
newtype Kernel src acc = Kernel (DIM2 -> src -> acc -> acc)

-- | Some kernels can be factorized in two uni-dimensional kernels (horizontal
-- and vertical).
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
data FilterFold1

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

-- | Box filters initialized with a given value.
instance (Image src, FromFunction res, src_p ~ ImagePixel src
        , res_p ~ FromFunctionPixel res)
        => Filterable src res (BoxFilter src_p (FilterFold acc) acc res_p) where
    apply !img !(Filter ksize anchor (Kernel kernel) ini post interpol) =
        let !size@(Z :. ih :. iw) = shape img
            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize
            !(FilterFold acc)  = ini
        in fromFunction size $ \(!(Z :. iy :. ix)) ->
                post $! boxGoColumn img kernel interpol ih iw (iy - kcy)
                                    (ix - kcx) kh kw 0 acc
    {-# INLINE apply #-}

-- | Box filters initialized using the first pixel of the kernel.
instance (Image src, FromFunction res, acc ~ ImagePixel src
        , res_p ~ FromFunctionPixel res)
        => Filterable src res (BoxFilter acc FilterFold1 acc res_p) where
    apply !img !(Filter ksize anchor (Kernel kernel) _ post interpol) =
        let !size@(Z :. ih :. iw) = shape img
            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize
        in fromFunction size $ \(!(Z :. iy :. ix)) ->
                post $! boxGoColumn1 img kernel interpol ih iw (iy - kcy)
                                     (ix - kcx) kh kw
    {-# INLINE apply #-}

boxGoColumn :: Image src => src -> (DIM2 -> ImagePixel src -> acc -> acc)
            -> BorderInterpolate (ImagePixel src) -> Int-> Int -> Int -> Int
            -> Int -> Int -> Int -> acc -> acc
boxGoColumn !img !kernel !interpol !ih !iw !iy !ix !kh !kw !ky !acc
    | ky < kh   =
        case borderInterpolate interpol ih iy of
            Left  iy' -> boxGoLine img kernel interpol ih iw iy (iy' * iw) ix ix
                                   kh kw ky 0 acc
            Right val -> boxGoLineConst img kernel interpol ih iw iy ix kh kw ky
                                        0 val acc
    | otherwise = acc

boxGoColumn1 :: Image src => src
             -> (DIM2 -> ImagePixel src -> ImagePixel src -> ImagePixel src)
             -> BorderInterpolate (ImagePixel src) -> Int -> Int -> Int -> Int
             -> Int -> Int -> ImagePixel src
boxGoColumn1 !img !kernel !interpol !ih !iw !iy !ix !kh !kw
    | kh > 0 && kw > 0 =
        case borderInterpolate interpol ih iy of
            Left  iy' ->
                let !linearIY = iy' * iw
                    !acc      = img `linearIndex` linearIY
                in boxGoLine img kernel interpol ih iw iy linearIY ix (ix + 1)
                             kh kw 0 1 acc
            Right val -> boxGoLineConst img kernel interpol ih iw iy ix kh kw 0
                                        1 val val
    | otherwise = error "Using FilterFold1 with an empty kernel."

boxGoLine :: Image src => src -> (DIM2 -> ImagePixel src -> acc -> acc)
          -> BorderInterpolate (ImagePixel src) -> Int -> Int -> Int -> Int
          -> Int -> Int -> Int -> Int -> Int -> Int -> acc -> acc
boxGoLine !img !kernel !interpol !ih !iw !iy !linearIY !ix0 !ix !kh !kw !ky !kx
          !acc
    | kx < kw   =
        let !val = case borderInterpolate interpol iw ix of
                        Left  ix'  -> img `linearIndex` (linearIY + ix')
                        Right val' -> val'
            !acc' = kernel (ix2 ky kx) val acc
        in boxGoLine img kernel interpol ih iw iy linearIY ix0 (ix + 1) kh kw ky
                     (kx + 1) acc'
    | otherwise = boxGoColumn img kernel interpol ih iw (iy + 1) ix0 kh kw 
                              (ky + 1) acc

boxGoLineConst :: Image src => src -> (DIM2 -> ImagePixel src -> acc -> acc)
               -> BorderInterpolate (ImagePixel src) -> Int -> Int -> Int -> Int
               -> Int -> Int -> Int -> Int -> ImagePixel src -> acc -> acc
boxGoLineConst !img !kernel !interpol !ih !iw !iy !ix !kh !kw !ky !kx !val !acc
    | kx < kw   = let !acc' = kernel (ix2 ky kx) val acc
                  in boxGoLineConst img kernel interpol ih iw iy ix kh kw ky
                                    (kx + 1) val acc'
    | otherwise = boxGoColumn img kernel interpol ih iw (iy + 1) ix kh kw
                              (ky + 1) acc

instance (Image src, FromFunction res, SeparatelyFiltrable src res acc
        , src_p ~ ImagePixel src, res_p ~ FromFunctionPixel res
        , FromFunction      (SeparableFilterAccumulator src res acc)
        , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
        , Image             (SeparableFilterAccumulator src res acc)
        , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
        => Filterable src res (SeparableFilter src_p (FilterFold acc) acc res_p)
            where
    apply !src !f =
        fst $! wrapper src f
      where
        wrapper :: (Image src, FromFunction res
            , FromFunction (SeparableFilterAccumulator src res acc)
            , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
            , Image             (SeparableFilterAccumulator src res acc)
            , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
            => src
            -> SeparableFilter (ImagePixel src) (FilterFold acc) acc
                               (FromFunctionPixel res)
            -> (res, SeparableFilterAccumulator src res acc)
        wrapper !src !(Filter ksize anchor kernel ini post interpol) =
            (res, tmp)
          where
            !size@(Z :. ih :. iw) = shape src

            !(Z :. kh  :. kw)  = ksize
            !(Z :. kcy :. kcx) = kernelAnchor anchor ksize

            !(SeparableKernel vert horiz) = kernel
            !(FilterFold acc)             = ini

            !tmp = fromFunction size $ \(!(Z :. iy :. ix)) ->
                        let !iy0 = iy - kcy
                        in if iy0 >= 0 && iy0 + kh <= ih
                              then goColumnSafe iy0 ix 0 acc
                              else goColumn     iy0 ix 0 acc

            !res = fromFunction size $ \(!(Z :. iy :. ix)) ->
                        let !ix0 = ix - kcx
                        in post $! if ix0 >= 0 && ix0 + kw <= iw
                                        then goLineSafe (iy * iw) ix0 0 acc
                                        else goLine     (iy * iw) ix0 0 acc

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
                        foldl' (\acc ky -> vert (ix1 ky) val acc) acc [0..kh-1]
                      | otherwise                      = undefined
        {-# INLINE wrapper #-}
    {-# INLINE apply #-}

instance (Image src, FromFunction res, SeparatelyFiltrable src res acc
        , src_p ~ ImagePixel src, res_p ~ FromFunctionPixel res
        , acc ~ ImagePixel src
        , FromFunction      (SeparableFilterAccumulator src res acc)
        , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
        , Image             (SeparableFilterAccumulator src res acc)
        , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
        => Filterable src res (SeparableFilter src_p FilterFold1 acc res_p)
            where
    apply !src !f =
        fst $! wrapper src f
      where
        wrapper :: (Image src, FromFunction res, acc ~ ImagePixel src
            , FromFunction (SeparableFilterAccumulator src res acc)
            , FromFunctionPixel (SeparableFilterAccumulator src res acc) ~ acc
            , Image             (SeparableFilterAccumulator src res acc)
            , ImagePixel        (SeparableFilterAccumulator src res acc) ~ acc)
            => src
            -> SeparableFilter (ImagePixel src) FilterFold1 acc
                               (FromFunctionPixel res)
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

            !res = fromFunction size $ \(!(Z :. iy :. ix)) ->
                        let !ix0 = ix - kcx
                        in post $! if ix0 >= 0 && ix0 + kw <= iw
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
                    !acc      = src `linearIndex` linearIY
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
                    let !val  = src `linearIndex` linearIY
                        !acc' = vert (ix1 ky) val acc
                    in goColumnSafe (linearIY + iw) ix (ky + 1) acc'
                | otherwise = acc

            goLine1 !linearIY !ix =
                let !acc =
                        case borderInterpolate interpol iw ix of
                            Left  ix'-> tmp `linearIndex` (linearIY + ix')
                            Right _  -> columnConst
                in goLine linearIY (ix + 1) 1 acc

            goLine1Safe !linearIY !ix =
                let !linearIX = linearIY + ix
                    !acc      = src `linearIndex` linearIX
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

-- erode :: Ord input => Filter input input input
-- erode = Filter (ix2 3 3) KernelAnchorCenter
--                (SeparableKernel (const min) (const min)) FilterFold1 id

blur :: Integral a => Int -> BoxFilter a (FilterFold Int) Int a
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

gaussianBlur :: Integral a => Int -> Maybe Float
             -> SeparableFilter a (FilterFold Float) Float a
gaussianBlur !radius !mSig =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel vert horiz)
           (FilterFold 0) (round . (/ kernelSum)) BorderReplicate
  where
    !size = radius * 2 + 1

    -- If σ is not provided, tries to fit 3σ in the kernel.
    !sig = case mSig of Just s  -> s
                        Nothing -> (0.5 + fromIntegral radius) / 3

    vert !(Z :. y) !val !acc = let !coeff = kernelVec V.! y
                            in acc + fromIntegral val * coeff
    {-# INLINE vert #-}
    horiz !(Z :. x) !acc1 !acc2 = let !coeff = kernelVec V.! x
                               in acc1 * coeff + acc2
    {-# INLINE horiz #-}

    !kernelVec =
        V.generate size $ \x ->
            gaussian $! fromIntegral $! abs $! x - radius
    !kernelSum = square (V.sum kernelVec)

    gaussian !x = invSigSqrt2Pi * exp (inv2xSig2 * square x)

    -- Pre-computed terms of the Gaussian function.
    !invSigSqrt2Pi = 1 / (sig * sqrt (2 * pi))
    !inv2xSig2     = -1 / (2 * square sig)
{-# INLINE gaussianBlur #-}

data Derivative = DerivativeX | DerivativeY

scharr :: (Integral a, Num b) => BoxFilter a (FilterFold b) b b
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

sobel :: (Integral src, Integral acc, Storable acc) => Int -> Derivative
      -> SeparableFilter src (FilterFold acc) acc acc
sobel radius der =
    let !kernel =
            case der of
                DerivativeX -> SeparableKernel (vert vec1) (horiz vec2)
                DerivativeY -> SeparableKernel (vert vec2) (horiz vec1)
    in Filter (ix2 size size) KernelAnchorCenter kernel (FilterFold 0) id
              BorderReplicate
  where
    !size = radius * 2 + 1

    vert !vec !(Z :. y) !val !acc = let !coeff = vec V.! y
                                    in acc + fromIntegral val * coeff
    {-# INLINE vert #-}
    horiz !vec !(Z :. x) !acc1 !acc2 = let !coeff = vec V.! x
                                       in acc1 * coeff + acc2
    {-# INLINE horiz #-}

    !radius' = fromIntegral radius
    !vec1 = let pows = [ 2^i | i <- [0..radius'] ]
            in V.fromList $ pows ++ (tail (reverse pows))
    !vec2 = V.fromList $ [1..radius'] ++ [0] ++ map negate [1..radius']

square :: Num a => a -> a
square a = a * a

word :: Integral a => a -> Word
word = fromIntegral
