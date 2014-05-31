module Vision.Detector.Edge (canny) where

import Data.Int

import Vision.Image.Interpolate (bilinearInterpol)

-- Detects edges using the Canny's algorithm.
-- See <http://en.wikipedia.org/wiki/Canny_edge_detector> for details.
-- canny :: (Image i, ImagePixel i ~ GreyPixel) => i
canny !img !radius !mSig !lowThres !highThres=
    let blurred        :: GreyImage
        gaussianFilter :: SeparableFilter src Float res
        !gaussianFilter = gaussianBlur radius mSig
        !blurred        = img `I.apply` gaussianFilter

        dx, dy :: Manifest Int16
        !dx = img `I.apply` scharr DerivativeX
        !dy = img `I.apply` scharr DerivativeY

        -- Magnitude of the gradient, squared.
        dxy :: Manifest Int32
        !dxy = fromFunction (shape img) $ \!pt ->
                    square (dx `index` pt) + square (dy `index` pt)

        -- Direction of the gradient.
        angle :: Manifest Float
        !angle = fromFunction (shape img) $ \!pt ->
                    atan2 (dx `index` pt) (dy `index` pt)
    in

  where
    isMaxima !x !y magn !dx !dy =
        let !dx'  = dx % magn
            !dy'  = dy % magn
            !x'   = ratio x
            !y'   = ratio y
            !left = RPoint (x' + dx') (y' + dy')
            right = RPoint (x' - dx') (y' - dy')
        in    dxy `bilinearInterpol` left  < magn
           && dxy `bilinearInterpol` right < magn

square :: Num a => a -> a
square a = a * a

ratio :: Integral a => a -> RatioInt
ratio = fromIntegral

