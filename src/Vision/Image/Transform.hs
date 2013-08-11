{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | Provides high level functions to manipulate images.
module Vision.Image.Transform (
      InterpolMethod (..), crop, resize, horizontalFlip, verticalFlip
    ) where

import Data.Array.Repa (D, DIM2, Source, Z (..), (:.) (..))
import Data.RatioInt (RatioInt, (%))

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Function (extent, extract)
import Vision.Image.Interpolate (RPoint (..), Interpolable, bilinearInterpol)
import Vision.Image.Primitive (Rect (..))

-- | Defines the set of possible methods for pixel interpolations when looking
-- for a pixel at floating point coordinates.
data InterpolMethod =
      TruncateInteger -- ^ Selects the top left pixel (fastest).
    | NearestNeighbor -- ^ Selects the nearest pixel (fast).
    | Bilinear        -- ^ Does a double linear interpolation over the four
                      -- surrounding points (slow).

-- | Maps the content of the image\'s rectangle in a new image.
crop :: (Image i, Source r (Channel i)) => i r -> Rect -> i D
crop !img !(Rect rx ry rw rh) = extract (Z :. ry :. rx) (Z :. rh :. rw) img
{-# INLINE crop #-}

-- | Resizes the 'Image' using the given interpolation method.
resize :: (FromFunction i, Interpolable i, Source r (Channel i))
       => i r -> InterpolMethod -> DIM2 -> i (FunctionRepr i)
resize img !method !size'@(Z :. h' :. w') =
    case method of
        TruncateInteger ->
            -- Interpolates the index of the middle of the corresponding pixel
            -- in the source image.
            let !widthRatio   = double w / double w'
                !widthMiddle  = (widthRatio - 1) / 2
                !heightRatio  = double h / double h'
                !heightMiddle = (heightRatio - 1) / 2
                line (Z :. y') = truncate $   double y' * heightRatio
                                            + heightMiddle
                pixel y (_ :. x') =
                    let !x = truncate $ double x' * widthRatio + widthMiddle
                    in img `getPixel` (Z :. y :. x)
            in fromFunctionLine size' line pixel
        NearestNeighbor ->
            let !widthRatio   = double w / double w'
                !widthMiddle  = (widthRatio - 1) / 2
                !heightRatio  = double h / double h'
                !heightMiddle = (heightRatio - 1) / 2
                line (Z :. y') = round $ double y' * heightRatio + heightMiddle
                pixel y (_ :. x') =
                    let !x = round $ double x' * widthRatio + widthMiddle
                    in img `getPixel` (Z :. y :. x)
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
                line (Z :. y') = bound maxHeight $   ratio y' * heightRatio
                                                   + heightMiddle
                pixel y (_ :. x') =
                    let !x = bound maxWidth $   ratio x' * widthRatio 
                                              + widthMiddle
                    in img `bilinearInterpol` RPoint x y
            in fromFunctionLine size' line pixel
  where
    !(Z :. h :. w) = extent img
{-# INLINE resize #-}

-- | Reverses the image horizontally.
horizontalFlip :: (FromFunction i, Source r (Channel i)) => i r
               -> i (FunctionRepr i)
horizontalFlip !img =
    fromFunction size $ \(line :. x') ->
        let !x = maxX - x'
        in img `getPixel` (line :. x)
  where
    !size@(_ :. w) = extent img
    !maxX = w - 1
{-# INLINABLE horizontalFlip #-}

-- | Reverses the image vertically.
verticalFlip :: (FromFunction i, Source r (Channel i)) => i r
             -> i (FunctionRepr i)
verticalFlip !img =
    let line (Z :. y') = maxY - y'
        pixel y (_ :. x) = img `getPixel` (Z :. y :. x)
    in fromFunctionLine size line pixel
  where
    !size@(Z :. h :. _) = extent img
    !maxY = h - 1
{-# INLINABLE verticalFlip #-}

double :: Integral a => a -> Double
double = fromIntegral
ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
