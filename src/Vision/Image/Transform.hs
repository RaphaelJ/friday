{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

-- | Provides high level functions to do geometric transformations on images.
--
-- Each transformation has been declared INLINABLE so new image types could be
-- specialized.
module Vision.Image.Transform (
      InterpolMethod (..), crop, resize, horizontalFlip, verticalFlip, floodFill
    ) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (..))
import Data.RatioInt (RatioInt, (%))
import Data.Vector.Storable (enumFromN)

import Vision.Image.Interpolate (Interpolable, bilinearInterpol)
import Vision.Image.Mutable (MutableImage (Freezed, mShape, linearRead, linearWrite))
import Vision.Image.Type (
      MaskedImage (..), Image (..), ImageChannel, FromFunction (..)
    )
import Vision.Primitive (
      Z (..), (:.) (..), Point, RPoint (..), Rect (..), Size, ix2, toLinearIndex
    )

-- | Defines the set of possible methods for pixel interpolations when looking
-- for a pixel at floating point coordinates.
data InterpolMethod =
      TruncateInteger -- ^ Selects the top left pixel (fastest).
    | NearestNeighbor -- ^ Selects the nearest pixel (fast).
    | Bilinear        -- ^ Does a double linear interpolation over the four
                      -- surrounding points (slow).

-- | Maps the content of the image\'s rectangle in a new image.
crop :: (Image i1, FromFunction i2, ImagePixel i1 ~ FromFunctionPixel i2)
     => i1 -> Rect -> i2
crop !img !(Rect rx ry rw rh) =
    fromFunction (Z :. rh :. rw) $ \(Z :. y :. x) ->
        img `index` ix2 (ry + y) (rx + x)
{-# INLINABLE crop #-}

-- | Resizes the 'Image' using the given interpolation method.
resize :: (Image i1, Interpolable (ImagePixel i1), FromFunction i2
         , ImagePixel i1 ~ FromFunctionPixel i2, Integral (ImageChannel i1))
       => i1 -> InterpolMethod -> Size -> i2
resize !img !method !size'@(Z :. h' :. w') =
    case method of
        TruncateInteger ->
            let !widthRatio   = double w / double w'
                !widthMiddle  = (widthRatio - 1) / 2
                !heightRatio  = double h / double h'
                !heightMiddle = (heightRatio - 1) / 2
                line !y' = truncate $ double y' * heightRatio + heightMiddle
                {-# INLINE line #-}
                col  !x' = truncate $ double x' * widthRatio  + widthMiddle
                {-# INLINE col #-}
                f !y !(Z :. _ :. x') = let !x = col x'
                                       in img `index` ix2 y x
                {-# INLINE f #-}
            in fromFunctionLine size' line f
        NearestNeighbor ->
            let !widthRatio   = double w / double w'
                !widthMiddle  = (widthRatio - 1) / 2
                !heightRatio  = double h / double h'
                !heightMiddle = (heightRatio - 1) / 2
                line !y' = round $ double y' * heightRatio + heightMiddle
                {-# INLINE line #-}
                col  !x' = round $ double x' * widthRatio  + widthMiddle
                {-# INLINE col #-}
                f !y !(Z :. _ :. x') = let !x = col x'
                                       in img `index` ix2 y x
                {-# INLINE f #-}
            in fromFunctionLine size' line f
        Bilinear ->
            let !widthRatio  = w % w'
                !widthMiddle = (widthRatio - 1) / 2
                !maxWidth = ratio (w - 1)
                !heightRatio = (h - 1) % (h' - 1)
                !heightMiddle = (heightRatio - 1) / 2
                !maxHeight = ratio (h - 1)
                -- Limits the interpolation to inner pixel as first and last
                -- pixels can have out of bound coordinates.
                bound !limit = min limit . max 0
                {-# INLINE bound #-}
                line !y' = bound maxHeight $   ratio y' * heightRatio
                                             + heightMiddle
                {-# INLINE line #-}
                col  !x' = bound maxWidth  $   ratio x' * widthRatio
                                             + widthMiddle
                {-# INLINE col #-}
                f !y !x _ = img `bilinearInterpol` RPoint x y
                {-# INLINE f #-}
            in fromFunctionCached size' line col f
  where
    !(Z :. h :. w) = shape img
{-# INLINABLE resize #-}

-- | Reverses the image horizontally.
horizontalFlip :: (Image i1, FromFunction i2
                  , ImagePixel i1 ~ FromFunctionPixel i2)
               => i1 -> i2
horizontalFlip !img =
    let f !(Z :. y :. x') = let !x = maxX - x'
                            in img `index` ix2 y x
        {-# INLINE f #-}
    in fromFunction size f
  where
    !size@(Z :. _ :. w) = shape img
    !maxX = w - 1
{-# INLINABLE horizontalFlip #-}

-- | Reverses the image vertically.
verticalFlip :: (Image i1, FromFunction i2
                , ImagePixel i1 ~ FromFunctionPixel i2)
             => i1 -> i2
verticalFlip !img =
    let line !y' = maxY - y'
        {-# INLINE line #-}
        f !y !(Z :. _ :. x) = img `index` ix2 y x
        {-# INLINE f #-}
    in fromFunctionLine size line f
  where
    !size@(Z :. h :. _) = shape img
    !maxY = h - 1
{-# INLINABLE verticalFlip #-}

floodFill :: (PrimMonad m, MutableImage i, Eq (ImagePixel (Freezed i)))
          => Point -> i (PrimState m) -> ImagePixel (Freezed i) -> m ()
floodFill !pt !img newVal = do
    let !linearIX = toLinearIndex size pt
    val <- linearRead img linearIX
    go val pt linearIX
  where
    !size@(Z :. h :. w) = mShape img

    go !val !(Z :. y :. x) linearIX = do
        let !minLineLinearIX = linearIX - x
            !maxLineLinearIX = minLineLinearIX + w - 1

        pix <- linearRead img linearIX

        when (pix == val) $ do
            linearWrite img linearIX newVal

            stopLeft  <- goHoriz val (< minLineLinearIX) pred (linearIX - 1)
            stopRight <- goHoriz val (> maxLineLinearIX) succ (linearIX + 1)
            return ()

            let !from  = stopLeft  + 1
                !to    = stopRight - 1
                !xFrom = x - (linearIX - from)

            when (y > 0) $
                visitLine val to (ix2 (y - 1) xFrom) from
            when ((y + 1) < h) $
                visitLine val to (ix2 (y + 1) xFrom) from

    goHoriz !val !stop !next !linearIX
        | stop linearIX = return linearIX
        | otherwise     = do
            pix <- linearRead img linearIX
            if pix == val then do linearWrite img linearIX newVal
                                  goHoriz val stop next (next linearIX)
                          else return linearIX

    visitLine !val !maxLinearIX !pt@(y :. x) !linearIX
        | linearIX > maxLinearIX = return ()
        | otherwise              = do
            go val pt linearIX
            visitLine val maxLinearIX (y :. (x + 1)) (linearIX + 1)
{-# INLINABLE floodFill #-}

double :: Integral a => a -> Double
double = fromIntegral
ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
