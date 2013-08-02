{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.GreyImage.Conversion (
      Convertible (..), convert, rgbToGrey
    ) where

import Data.Array.Repa (D, Source)
import Data.Convertible (Convertible (..), convert)
import Data.Word

import Vision.Image.Class (Image (..), fromFunction)
import Vision.Image.Function (extent)
import Vision.Image.GreyImage.Type (GreyImage (..), GreyPixel (..))
import Vision.Image.RGBAImage.Type (RGBAImage (..), RGBAPixel (..))
import Vision.Image.RGBImage.Type (RGBImage (..), RGBPixel (..))

instance Convertible (GreyImage r) (GreyImage r) where
    safeConvert = Right
    {-# INLINE safeConvert #-}

-- | Converts a RGBA image to greyscale.
instance (Source r (Channel RGBAImage))
      => Convertible (RGBAImage r) (GreyImage D) where
    safeConvert img =
        Right $ fromFunction size $ \idx ->
            let RGBAPixel r g b a = img `getPixel` idx
            in GreyPixel $ word8 $ rgbToGrey r g b * int a `quot` 255
      where
        size = extent img
    {-# INLINE safeConvert #-}

-- | Converts a RGB image to a delayed greyscale.
instance (Source r (Channel RGBImage))
      => Convertible (RGBImage r) (GreyImage D) where
    safeConvert img =
        Right $ fromFunction size $ \idx ->
            let RGBPixel r g b = img `getPixel` idx
            in GreyPixel $ word8 $ rgbToGrey r g b
      where
        size = extent img
    {-# INLINE safeConvert #-}

-- | Converts the colors to greyscale using the human eye colors perception.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Int
rgbToGrey r g b = (int r * 30 + int g * 59 + int b * 11) `quot` 100
{-# INLINE rgbToGrey #-}

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
