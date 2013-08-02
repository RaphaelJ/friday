{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | Provides high level functions to manipulate images.
module Vision.Image.Transform (
      InterpolMethod (..), crop, resize, horizontalFlip, verticalFlip
    ) where

import Data.Array.Repa (DIM2, Source, Z (..), (:.) (..))
import Data.RatioInt (RatioInt, (%))

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Function (extent)
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
crop :: (FromFunction i, Source r (Channel i)) => i r -> Rect
     -> i (FunctionRepr i)
crop img !(Rect rx ry rw rh) =
    fromFunction (Z :. rh :. rw) $ \(Z :. y :. x) ->
        img `getPixel` (Z :. ry + y :. rx + x)
{-# INLINE crop #-}

-- | Resizes the 'Image' using the given interpolation method.
resize :: (FromFunction i, Interpolable i, Source r (Channel i))
       => i r -> InterpolMethod -> DIM2 -> i (FunctionRepr i)
resize img !method !size'@(Z :. h' :. w') =
    case method of
        TruncateInteger ->
            -- FIXME: w' - 1 == 0
            let !widthRatio  = double (w - 1) / double (w' - 1)
                !heightRatio = double (h - 1) / double (h' - 1)
            in fromFunction size' $ \(Z :. y' :. x') ->
                let !x = truncate $ double x' * widthRatio
                    !y = truncate $ double y' * heightRatio
                in img `getPixel` (Z :. y :. x)
        NearestNeighbor ->
            let !widthRatio  = double (w - 1) / double (w' - 1)
                !heightRatio = double (h - 1) / double (h' - 1)
            in fromFunction size' $ \(Z :. y' :. x') ->
                let !x = round $ double x' * widthRatio
                    !y = round $ double y' * heightRatio
                in img `getPixel` (Z :. y :. x)
        Bilinear ->
            let !widthRatio  = (w - 1) % (w' - 1)
                !heightRatio = (h - 1) % (h' - 1)
            in fromFunction size' $ \(Z :. y' :. x') ->
                let !x = ratio x' * widthRatio
                    !y = ratio y' * heightRatio
                in img `bilinearInterpol` RPoint x y
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
    fromFunction size $ \(Z :. y' :. x) ->
        let !y = maxY - y'
        in img `getPixel` (Z :. y :. x)
  where
    !size@(Z :. h :. _) = extent img
    !maxY = h - 1
{-# INLINABLE verticalFlip #-}

double :: Integral a => a -> Double
double = fromIntegral
ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
