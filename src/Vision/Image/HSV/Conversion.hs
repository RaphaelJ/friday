{-# LANGUAGE BangPatterns
           , MultiParamTypeClasses
           , PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.HSV.Conversion () where

import Data.Convertible (Convertible (..), ConvertResult)
import Data.Word

import Vision.Image.HSV.Type (HSVPixel (..))
import Vision.Image.RGB.Type (RGBPixel (..))
import Vision.Image.RGB.Conversion ()
import Vision.Image.RGBA.Type (RGBAPixel (..))
import Vision.Image.RGBA.Conversion ()

instance Convertible HSVPixel HSVPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel HSVPixel where
-- Based on :
-- http://en.wikipedia.org/wiki/HSL_and_HSV#General_approach
    safeConvert !(RGBPixel r g b) =
        Right pix
      where
        (!r', !g', !b') = (int r, int g, int b)

        !pix | r >= g && r >= b = -- r == max r g b
                let !c = r' - min b' g'
                    !h = fixHue $ hue c b' g' -- Hue can be negative
                in HSVPixel (word8 h) (sat c r') r
             | g >= r && g >= b = -- g == max r g b
                let !c = g' - min r' b'
                    !h = 60 + hue c r' b'
                in HSVPixel (word8 h) (sat c g') g
             | otherwise = -- b == max r g b
                let !c = b' - min r' g'
                    !h = 120 + hue c g' r'
                in HSVPixel (word8 h) (sat c b') b

        -- Returns a value in [-30; +30].
        hue 0  _      _     = 0
        hue !c !left !right = (30 * (right - left)) `quot` c

        sat _  0 = 0
        sat !c v = word8 $ (c * 255) `quot` v

        -- Keeps the value of the hue between [0, 179].
        -- As the Hue's unit is 2°, 180 is equal to 360° and to 0.
        fixHue !h | h < 0     = h + 180
                  | otherwise = h

instance Convertible HSVPixel RGBPixel where
-- Based on :
-- http://en.wikipedia.org/wiki/HSL_and_HSV#Converting_to_RGB
    safeConvert !(HSVPixel h s v) =
        Right $! case h `quot` 30 of
                0 -> RGBPixel v                (word8 x1')      (word8 m)
                1 -> RGBPixel (word8 (x2 60))  v                (word8 m)
                2 -> RGBPixel (word8 m)        v                (word8 (x1 60))
                3 -> RGBPixel (word8 m)        (word8 (x2 120)) v
                4 -> RGBPixel (word8 (x1 120)) (word8 m)        v
                5 -> RGBPixel v                (word8 m)        (word8 (x2 180))
                _ -> error "Invalid hue value."
      where
        (!h', v') = (int h, int v)

        -- v is the major color component whereas m is the minor one.
        !m = (v' * (255 - int s)) `quot` 255

        -- Computes the remaining component by resolving the hue equation,
        -- knowing v and m. x1 is when the component is on the right of the
        -- major one, x2 when on the left.
        x1 d = (d * m - d * v' + h' * v' - h' * m + 30 * m) `quot` 30
        x1'  = (                 h' * v' - h' * m + 30 * m) `quot` 30 -- == x1 0

        x2 d = (d * v' - d * m + h' * m - h' * v' + 30 * m) `quot` 30
    {-# INLINE safeConvert #-}

instance Convertible RGBAPixel HSVPixel where
    safeConvert pix = (safeConvert pix :: ConvertResult RGBPixel)
                      >>= safeConvert

instance Convertible HSVPixel RGBAPixel where
    safeConvert pix = (safeConvert pix :: ConvertResult RGBPixel)
                      >>= safeConvert

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
