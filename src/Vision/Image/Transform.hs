{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

-- | Provides high level functions to manipulate images.
module Vision.Image.Transform (
      InterpolMethod (..), crop, resize, horizontalFlip, verticalFlip
    ) where

import Data.RatioInt (RatioInt, (%))

import Vision.Image.Interpolate (Interpolable, bilinearInterpol)
import Vision.Image.Type (Image (..), FromFunction (..))
import Vision.Primitive (Z (..), (:.) (..), RPoint (..), Rect (..), Size, ix2)

-- | Defines the set of possible methods for pixel interpolations when looking
-- for a pixel at floating point coordinates.
data InterpolMethod =
      TruncateInteger -- ^ Selects the top left pixel (fastest).
    | NearestNeighbor -- ^ Selects the nearest pixel (fast).
    | Bilinear        -- ^ Does a double linear interpolation over the four
                      -- surrounding points (slow).

-- | Maps the content of the image\'s rectangle in a new image.
crop :: (Image i1, FromFunction i2, ImagePixel i1 ~ ImagePixel i2)
     => i1 -> Rect -> i2
crop !img !(Rect rx ry rw rh) =
    fromFunction (Z :. rh :. rw) $ \(Z :. y :. x) ->
        img `index` ix2 (ry + y) (rx + x)
{-# INLINABLE crop #-}

-- | Resizes the 'Image' using the given interpolation method.
resize :: (Image i1, Interpolable (ImagePixel i1), FromFunction i2
          , ImagePixel i1 ~ ImagePixel i2)
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
                f !y !(Z :. _ :. x') =
                    let !x = truncate $ double x' * widthRatio + widthMiddle
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
                f !y !(Z :. _ :. x') =
                    let !x = round $ double x' * widthRatio + widthMiddle
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
                line !y' = bound maxHeight $   ratio y' * heightRatio
                                             + heightMiddle
                {-# INLINE line #-}
                f !y !(Z :. _ :. x') =
                    let !x = bound maxWidth $   ratio x' * widthRatio
                                              + widthMiddle
                    in img `bilinearInterpol` RPoint x y
                {-# INLINE f #-}
            in fromFunctionLine size' line f
  where
    !(Z :. h :. w) = shape img
{-# INLINABLE resize #-}

-- | Reverses the image horizontally.
horizontalFlip :: (Image i1, FromFunction i2, ImagePixel i1 ~ ImagePixel i2)
               => i1 -> i2
horizontalFlip !img =
    fromFunction size $ \(Z :. y :. x') ->
        let !x = maxX - x'
        in img `index` ix2 y x
  where
    !size@(Z :. _ :. w) = shape img
    !maxX = w - 1
{-# INLINABLE horizontalFlip #-}

-- | Reverses the image vertically.
verticalFlip :: (Image i1, FromFunction i2, ImagePixel i1 ~ ImagePixel i2)
             => i1 -> i2
verticalFlip !img =
    let line !y' = maxY - y'
        f !y !(Z :. _ :. x) = img `index` ix2 y x
    in fromFunctionLine size line f
  where
    !size@(Z :. h :. _) = shape img
    !maxY = h - 1
{-# INLINABLE verticalFlip #-}

double :: Integral a => a -> Double
double = fromIntegral
ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
