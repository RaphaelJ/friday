{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.RGBImage.Storage (Convertible (..), convert) where

import Data.Array.Repa (D, U)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Convertible (Convertible (..), convert)

import Vision.Image.Function (computeS, copyS, delay)
import Vision.Image.RGBImage.Conversion ()
import Vision.Image.RGBImage.Type (RGBImage (..))
import Vision.Image.Storage (IOImage (..))

instance Convertible IOImage (RGBImage D) where
    safeConvert (GreyIOImage grey) = Right $ convert grey
    safeConvert (RGBAIOImage rgba) = Right $ convert rgba
    safeConvert (RGBIOImage  rgb)  = Right $ delay   rgb
    {-# INLINE safeConvert #-}

instance Convertible IOImage (RGBImage F) where
    safeConvert img =
        case img of
            GreyIOImage grey -> Right $ computeS $ toDelayed grey
            RGBAIOImage rgba -> Right $ computeS $ toDelayed rgba
            RGBIOImage  rgb  -> Right rgb
      where
        toDelayed :: Convertible i (RGBImage D) => i -> RGBImage D
        toDelayed = convert
        {-# INLINE toDelayed #-}
    {-# INLINE safeConvert #-}

instance Convertible (RGBImage F) IOImage where
    safeConvert = Right . RGBIOImage
    {-# INLINE safeConvert #-}

instance Convertible (RGBImage D) IOImage where
    safeConvert = Right . RGBIOImage . computeS
    {-# INLINE safeConvert #-}

instance Convertible (RGBImage U) IOImage where
    safeConvert = Right . RGBIOImage . copyS
    {-# INLINE safeConvert #-}
