{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | Provides high level functions to manipulate images.
module Vision.Image.Transform (InterpolMethod (..), crop, resize) where

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

crop :: (FromFunction i, Source r (Channel i)) => i r -> Rect
     -> i (FunctionRepr i)
crop !img !(Rect rx ry rw rh) =
    fromFunction (Z :. rh :. rw) $ \(Z :. y :. w) ->
        img `getPixel` (Z :. ry + y :. rx + x)
{-# INLINE crop #-}

-- | Resizes the 'Image' using the given interpolation method.
resize :: (FromFunction i, Interpolable i, Source r (Channel i))
       => InterpolMethod -> i r -> DIM2 -> i (FunctionRepr i)
resize !method !img !size'@(Z :. h' :. w') =
    case method of
        TruncateInteger ->
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

double :: Integral a => a -> Double
double = fromIntegral
ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
