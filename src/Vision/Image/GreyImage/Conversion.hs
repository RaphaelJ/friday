{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.GreyImage.Conversion () where

import Data.Convertible (Convertible (..))
import Data.Word

import Vision.Image.GreyImage.Type (GreyPixel (..))
import Vision.Image.RGBAImage.Type (RGBAPixel (..))
import Vision.Image.RGBImage.Type (RGBPixel (..))

instance Convertible GreyPixel GreyPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible RGBAPixel GreyPixel where
    safeConvert !(RGBAPixel r g b a) =
        Right $ GreyPixel $ word8 $ rgbToGrey r g b * int a `quot` 255
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel GreyPixel where
    safeConvert !(RGBPixel r g b) = Right $ GreyPixel $ word8 $ rgbToGrey r g b
    {-# INLINE safeConvert #-}

-- | Converts the colors to greyscale using the human eye colors perception.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Int
rgbToGrey !r !g !b = (int r * 30 + int g * 59 + int b * 11) `quot` 100

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
