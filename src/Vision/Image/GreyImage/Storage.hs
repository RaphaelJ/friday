{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.GreyImage.Storage (Convertible (..), convert) where

import Data.Convertible (Convertible (..), convert)

import Vision.Image.Class (Channel)
import Vision.Image.GreyImage.Conversion ()
import Vision.Image.GreyImage.Type (GreyImage (..))
import Vision.Image.Storage (IOImage (..))

instance Convertible IOImage GreyImage where
    safeConvert (GreyIOImage grey) = Right grey
    safeConvert (RGBAIOImage rgba) = Right $ convert rgba
    safeConvert (RGBIOImage  rgb)  = Right $ convert rgb
    {-# INLINE safeConvert #-}

instance Convertible GreyImage IOImage where
    safeConvert = Right . GreyIOImage
    {-# INLINE safeConvert #-}
