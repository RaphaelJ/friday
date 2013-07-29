{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.RGBAImage.Conversion (Convertible (..), convert) where

import Data.Array.Repa (D, Source, (:.) (..), (!), extent, fromFunction)
import Data.Convertible (Convertible (..), convert)

import Vision.Image.Class (Channel)
import Vision.Image.GreyImage.Type (GreyImage (..))
import Vision.Image.RGBAImage.Type (RGBAImage (..))
import Vision.Image.RGBImage.Type (RGBImage (..))

instance Convertible (RGBAImage r) (RGBAImage r) where
    safeConvert = Right
    {-# INLINE safeConvert #-}

-- | Converts a greyscale image to RGBA.
instance (Source r (Channel GreyImage))
      => Convertible (GreyImage r) (RGBAImage D) where
    safeConvert (GreyImage grey) =
        Right $ RGBAImage $ fromFunction (size :. 4) pixFromGrey
      where
        size :. ~1 = extent grey

        pixFromGrey (_ :. 3) = 255
        pixFromGrey (i :. _) = grey ! (i :. 0)
        {-# INLINE pixFromGrey #-}
    {-# INLINE safeConvert #-}

-- | Converts a RGB image to RGBA.
instance (Source r (Channel RGBImage))
      => Convertible (RGBImage r) (RGBAImage D) where
    safeConvert (RGBImage rgb) =
        Right $ RGBAImage $ fromFunction (size :. 4) pixFromRGB
      where
        size :. ~3 = extent rgb

        pixFromRGB (_ :. 3) = 255
        pixFromRGB i        = rgb ! i
        {-# INLINE pixFromRGB #-}
    {-# INLINE safeConvert #-}
