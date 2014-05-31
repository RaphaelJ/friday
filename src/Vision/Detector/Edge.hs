module Vision.Detector.Edge (canny) where

import Data.Int

import Vision.Image.Interpolate (bilinearInterpol)

-- Detects edges using the Canny's algorithm.
-- See <http://en.wikipedia.org/wiki/Canny_edge_detector> for details.
-- canny :: (Image i, ImagePixel i ~ GreyPixel) => i
canny !img !radius !mSig !lowThres !highThres=
    let !size@(Z :. h :. w) = shape img

        blurred        :: GreyImage
        gaussianFilter :: SeparableFilter src Float res
        !gaussianFilter = gaussianBlur radius mSig
        !blurred        = img `I.apply` gaussianFilter

        dx, dy :: Manifest Int16
        !dx = img `I.apply` scharr DerivativeX
        !dy = img `I.apply` scharr DerivativeY

        -- Magnitude of the gradient, squared.
        dxy :: Manifest Int32
        !dxy = fromFunction size $ \!pt ->
                    square (dx `index` pt) + square (dy `index` pt)

        -- Direction of the gradient.
        angle :: Manifest Float
        !angle = fromFunction size $ \!pt ->
                    atan2 (dx `index` pt) (dy `index` pt)

    edges = new' (h * w) 0
    forM_ (enumFromN 0 h) $ \y -> do
        forM_ (enumFromN 0 w) $ \x -> do
            
  where
    isMaxima !x !y !dxy !dx !dy =
        let dx'  = dx % magn
            dy'  = dy % magn
            x'   = ratio x
            y'   = ratio y
            left  = RPoint (x' + dx') (y' + dy')
            right = RPoint (x' - dx') (y' - dy')
        in if | left >= 0
            
            dxy `bilinearInterpol` left  < magn
           && dxy `bilinearInterpol` right < magn

square :: Num a => a -> a
square a = a * a

ratio :: Integral a => a -> RatioInt
ratio = fromIntegral

