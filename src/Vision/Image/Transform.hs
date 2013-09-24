{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
-- | Provides high level functions to manipulate images.
module Vision.Image.Transform (
      InterpolMethod (..), crop, resize, horizontalFlip, verticalFlip
    ) where

import Data.RatioInt (RatioInt, (%))

import Vision.Image.Interpolate (RPoint (..), Interpolable, bilinearInterpol)
import Vision.Image.Primitive (Point (..), Rect (..), Size (..))
import Vision.Image.Type (Image (..), FromFunction (..))

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
    fromFunction (Size rw rh) $ \(Point x y) ->
        img `getPixel` Point (rx + x) (ry + y)
{-# INLINABLE crop #-}

-- | Resizes the 'Image' using the given interpolation method.
resize :: (Image i1, Interpolable (ImagePixel i1), FromFunction i2
          , ImagePixel i1 ~ ImagePixel i2)
       => i1 -> InterpolMethod -> Size -> i2
resize img !method !size'@(Size w' h') =
    case method of
        TruncateInteger ->
            -- Interpolates the index of the middle of the corresponding pixel
            -- in the source image.
            let !widthRatio   = double w / double w'
                !widthMiddle  = (widthRatio - 1) / 2
                !heightRatio  = double h / double h'
                !heightMiddle = (heightRatio - 1) / 2
                line y' = truncate $ double y' * heightRatio + heightMiddle
                pixel y (Point x' _) =
                    let !x = truncate $ double x' * widthRatio + widthMiddle
                    in img `getPixel` Point x y
            in fromFunctionLine size' line pixel
        NearestNeighbor ->
            let !widthRatio   = double w / double w'
                !widthMiddle  = (widthRatio - 1) / 2
                !heightRatio  = double h / double h'
                !heightMiddle = (heightRatio - 1) / 2
                line y' = round $ double y' * heightRatio + heightMiddle
                pixel y (Point x' _) =
                    let !x = round $ double x' * widthRatio + widthMiddle
                    in img `getPixel` Point x y
            in fromFunctionLine size' line pixel
        Bilinear ->
            let !widthRatio  = w % w'
                !widthMiddle = (widthRatio - 1) / 2
                !maxWidth = ratio (w - 1)
                !heightRatio = (h - 1) % (h' - 1)
                !heightMiddle = (heightRatio - 1) / 2
                !maxHeight = ratio (h - 1)
                -- Limits the interpolation to inner pixel as first and last
                -- pixels can have out of bound coordinates.
                bound limit = min limit . max 0
                line y' = bound maxHeight $   ratio y' * heightRatio
                                            + heightMiddle
                pixel y (Point x' _) =
                    let !x = bound maxWidth $   ratio x' * widthRatio 
                                              + widthMiddle
                    in img `bilinearInterpol` RPoint x y
            in fromFunctionLine size' line pixel
  where
    !(Size w h) = getSize img
{-# INLINABLE resize #-}

-- | Reverses the image horizontally.
horizontalFlip :: (Image i1, FromFunction i2, ImagePixel i1 ~ ImagePixel i2)
               => i1 -> i2
horizontalFlip !img =
    fromFunction size $ \(Point x' y) ->
        let !x = maxX - x'
        in img `getPixel` Point x y
  where
    !size@(Size w _) = getSize img
    !maxX = w - 1
{-# INLINABLE horizontalFlip #-}

-- | Reverses the image vertically.
verticalFlip :: (Image i1, FromFunction i2, ImagePixel i1 ~ ImagePixel i2)
             => i1 -> i2
verticalFlip !img =
    let line y' = maxY - y'
        pixel y (Point x _) = img `getPixel` Point x y
    in fromFunctionLine size line pixel
  where
    !size@(Size _ h) = getSize img
    !maxY = h - 1
{-# INLINABLE verticalFlip #-}

double :: Integral a => a -> Double
double = fromIntegral
ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
