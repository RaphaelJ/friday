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
        Right $ GreyPixel $ truncate $ rgbToGrey r g b * double a / 255
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel GreyPixel where
    safeConvert !(RGBPixel r g b) =
        Right $ GreyPixel $ truncate $ rgbToGrey r g b
    {-# INLINE safeConvert #-}

-- | Converts the colors to greyscale using the human eye colors perception.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Double
rgbToGrey !r !g !b = double r * 0.30 + double g * 0.59 + double b * 0.11

double :: Integral a => a -> Double
double = fromIntegral
