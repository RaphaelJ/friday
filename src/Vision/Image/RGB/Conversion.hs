{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.RGB.Conversion () where

import Data.Convertible (Convertible (..))
import Data.Word

import Vision.Image.Grey.Type (GreyPixel (..))
import Vision.Image.RGBA.Type (RGBAPixel (..))
import Vision.Image.RGB.Type (RGBPixel (..))

instance Convertible RGBPixel RGBPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible GreyPixel RGBPixel where
    safeConvert !(GreyPixel pix) = Right $ RGBPixel pix pix pix
    {-# INLINE safeConvert #-}

instance Convertible RGBAPixel RGBPixel where
    safeConvert !(RGBAPixel r g b a) =
        Right $ RGBPixel (withAlpha r) (withAlpha g) (withAlpha b)
      where
        !a' = int a
        withAlpha !val = word8 $ int val * a' `quot` 255
        {-# INLINE withAlpha #-}
    {-# INLINE safeConvert #-}

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
