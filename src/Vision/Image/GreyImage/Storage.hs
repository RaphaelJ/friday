{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
           , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.GreyImage.Storage (Convertible (..), convert) where

import Data.Array.Repa (D, Source)
import Data.Convertible (Convertible (..), convert)

import Vision.Image.Class (Channel)
import Vision.Image.Function (delay)
import Vision.Image.GreyImage.Conversion ()
import Vision.Image.GreyImage.Type (GreyImage (..))
import Vision.Image.Storage (IOImage (..))

instance Convertible IOImage (GreyImage D) where
    safeConvert (GreyIOImage grey) = Right grey
    safeConvert (RGBAIOImage rgba) = Right $ convert rgba
    safeConvert (RGBIOImage  rgb)  = Right $ convert rgb
    {-# INLINE safeConvert #-}

instance Source r (Channel GreyImage) => Convertible (GreyImage r) IOImage where
    safeConvert = Right . GreyIOImage . delay
    {-# INLINE safeConvert #-}
