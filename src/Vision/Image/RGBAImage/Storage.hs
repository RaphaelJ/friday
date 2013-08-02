{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.RGBAImage.Storage (Convertible (..), convert) where

import Data.Array.Repa (D, U)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Convertible (Convertible (..), convert)

import Vision.Image.Function (computeS, copyS, delay)
import Vision.Image.RGBAImage.Conversion ()
import Vision.Image.RGBAImage.Type (RGBAImage (..))
import Vision.Image.Storage (IOImage (..))

instance Convertible IOImage (RGBAImage D) where
    safeConvert (GreyIOImage grey) = Right $ convert grey
    safeConvert (RGBAIOImage rgba) = Right $ delay   rgba
    safeConvert (RGBIOImage  rgb)  = Right $ convert rgb
    {-# INLINE safeConvert #-}

-- | Prefers this instance to the computation of an manifest image from
-- 'RGBAImage D' as it uses the original IO image if the source image is an
-- 'RGBAImage'.
instance Convertible IOImage (RGBAImage F) where
    safeConvert img =
        case img of
            GreyIOImage grey -> Right $ computeS $ toDelayed grey
            RGBAIOImage rgba -> Right rgba
            RGBIOImage  rgb  -> Right $ computeS $ toDelayed rgb
      where
        toDelayed :: Convertible i (RGBAImage D) => i -> RGBAImage D
        toDelayed = convert
        {-# INLINE toDelayed #-}
    {-# INLINE safeConvert #-}

instance Convertible (RGBAImage F) IOImage where
    safeConvert = Right . RGBAIOImage
    {-# INLINE safeConvert #-}

instance Convertible (RGBAImage D) IOImage where
    safeConvert = Right . RGBAIOImage . computeS
    {-# INLINE safeConvert #-}

instance Convertible (RGBAImage U) IOImage where
    safeConvert = Right . RGBAIOImage . copyS
    {-# INLINE safeConvert #-}
