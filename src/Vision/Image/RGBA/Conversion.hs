{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.RGBA.Conversion () where

import Data.Convertible (Convertible (..))

import Vision.Image.Grey.Type (GreyPixel (..))
import Vision.Image.RGBA.Type (RGBAPixel (..))
import Vision.Image.RGB.Type (RGBPixel (..))

instance Convertible RGBAPixel RGBAPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible GreyPixel RGBAPixel where
    safeConvert !(GreyPixel pix) = Right $ RGBAPixel pix pix pix 255
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel RGBAPixel where
    safeConvert !(RGBPixel r g b) = Right $ RGBAPixel r g b 255
    {-# INLINE safeConvert #-}
