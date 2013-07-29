{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.RGBImage.Conversion (Convertible (..), convert) where

import Data.Array.Repa (D, Source, (:.) (..), (!), extent, fromFunction)
import Data.Convertible (Convertible (..), convert)
import Data.Word

import Vision.Image.Class (Channel)
import Vision.Image.GreyImage.Type (GreyImage (..))
import Vision.Image.RGBAImage.Type (RGBAImage (..))
import Vision.Image.RGBImage.Type (RGBImage (..))

instance Convertible (RGBImage r) (RGBImage r) where
    safeConvert = Right
    {-# INLINE safeConvert #-}

-- | Converts a greyscale image to RGB.
instance (Source r (Channel RGBImage))
      => Convertible (GreyImage r) (RGBImage D) where
    safeConvert (GreyImage grey) =
        Right $ RGBImage $ fromFunction (size :. 3) pixFromGrey
      where
        size :. ~1 = extent grey

        pixFromGrey (i :. _) = grey ! (i :. 0)
        {-# INLINE pixFromGrey #-}
    {-# INLINE safeConvert #-}

-- | Converts a RGBA image to RGB.
instance (Source r (Channel RGBImage))
      => Convertible (RGBAImage r) (RGBImage D) where
    safeConvert (RGBAImage rgba) =
        Right $ RGBImage $ fromFunction (size :. 3) pixFromRGBA
      where
        size :. ~4 = extent rgba

        pixFromRGBA i@(i' :. _) =
            let alpha = int $ rgba ! (i' :. 3)
                val   = int $ rgba ! i
            in word8 $ val * alpha `quot` 255
    {-# INLINE safeConvert #-}

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
