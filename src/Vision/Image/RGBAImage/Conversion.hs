{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.RGBAImage.Conversion () where

import Data.Convertible (Convertible (..))

import Vision.Image.GreyImage.Type (GreyPixel (..))
import Vision.Image.RGBAImage.Type (RGBAPixel (..))
import Vision.Image.RGBImage.Type (RGBPixel (..))

instance Convertible RGBAPixel RGBAPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible GreyPixel RGBAPixel where
    safeConvert !(GreyPixel pix) = Right $ RGBAPixel pix pix pix 255
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel RGBAPixel where
    safeConvert !(RGBPixel r g b) = Right $ RGBAPixel r g b 255
    {-# INLINE safeConvert #-}
