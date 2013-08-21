{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.GreyImage.Conversion (
      Convertible (..), convert, rgbToGrey
    ) where

import Data.Convertible (Convertible (..), convert)
import Data.Word

import Vision.Image.Class (Image (..), fromFunction)
import Vision.Image.GreyImage.Type (GreyImage (..), GreyPixel (..))
import Vision.Image.RGBAImage.Type (RGBAImage (..), RGBAPixel (..))
import Vision.Image.RGBImage.Type (RGBImage (..), RGBPixel (..))

instance Convertible GreyImage GreyImage where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible RGBAImage GreyImage where
    safeConvert img =
        Right $ fromFunction (size img) $ \!pt ->
            let !(RGBAPixel r g b a) = img `getPixel` pt
            in GreyPixel $ word8 $ rgbToGrey r g b * int a `quot` 255
    {-# INLINE safeConvert #-}

instance Convertible RGBImage GreyImage where
    safeConvert img =
        Right $ fromFunction (size img) $ \!pt ->
            let !(RGBPixel r g b) = img `getPixel` pt
            in GreyPixel $ word8 $ rgbToGrey r g b
    {-# INLINE safeConvert #-}

-- | Converts the colors to greyscale using the human eye colors perception.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Int
rgbToGrey !r !g !b = (int r * 30 + int g * 59 + int b * 11) `quot` 100
{-# INLINE rgbToGrey #-}

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
