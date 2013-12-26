{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.HSVImage.Conversion () where

import Data.Convertible (Convertible (..))
import Data.Word

import Vision.Image.RGBImage.Type (RGBPixel (..))
import Vision.Image.HSVImage.Type (HSVPixel (..))

instance Convertible HSVPixel HSVPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel HSVPixel where
-- Based on :
-- http://docs.opencv.org/modules/imgproc/doc/miscellaneous_transformations.html
    safeConvert !(RGBPixel r g b) =
        Right pix
      where
        (!r', !g', !b') = (int r, int g, int b)

        !pix | r >= g && r >= b = -- r == max r g b
                let !delta = r' - min b' g'
                    !h = hue delta g' b' -- Hue can be negative
                in HSVPixel (word8 $ fixHue h) (sat delta r') r
             | g >= r && g >= b = -- g == max r g b
                let !delta = g' - min r' b'
                    !h = 60 + hue delta b' r'
                in HSVPixel (word8 h)          (sat delta g') g
             | otherwise = -- b == max r g b
                let !delta = b' - min r' g'
                    !h = 120 + hue delta r' g'
                in HSVPixel (word8 h)          (sat delta b') b

        -- Returns a value in [-30; +30].
        hue 0      _      _     = 0
        hue !delta !right !left = (30 * (right - left)) `quot` delta

        sat _      0 = 0
        sat !delta v = word8 $ (delta * 255) `quot` v

        -- Keeps the value of the hue between [0, 179].
        -- As the Hue's unit is 2°, 180 is equal to 360° and to 0.
        fixHue !h | h < 0     = h + 180
                  | otherwise = h

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
