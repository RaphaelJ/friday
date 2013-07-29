{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.GreyImage.Conversion (
      Convertible (..), convert, rgbToGrey
    ) where

import Data.Array.Repa (D, Source, (:.) (..), (!), extent, fromFunction)
import Data.Convertible (Convertible (..), convert)
import Data.Word

import Vision.Image.Class (Channel)
import Vision.Image.GreyImage.Type (GreyImage (..))
import Vision.Image.RGBAImage.Type (RGBAImage (..))
import Vision.Image.RGBImage.Type (RGBImage (..))

instance Convertible (GreyImage r) (GreyImage r) where
    safeConvert = Right
    {-# INLINE safeConvert #-}

-- | Converts a RGBA image to greyscale.
instance (Source r (Channel RGBAImage))
      => Convertible (RGBAImage r) (GreyImage D) where
    safeConvert (RGBAImage rgba) =
        Right $ GreyImage $ fromFunction (size :. 1) $ \(i :. ~1) ->
            let !r = rgba ! (i :. 0)
                !g = rgba ! (i :. 1)
                !b = rgba ! (i :. 2)
                !a = rgba ! (i :. 3)
            in word8 $ rgbToGrey r g b * int a `quot` 255
      where
        size :. ~4 = extent rgba
    {-# INLINE safeConvert #-}

-- | Converts a RGB image to greyscale.
instance (Source r (Channel RGBImage))
      => Convertible (RGBImage r) (GreyImage D) where
    safeConvert (RGBImage rgb) =
        Right $ GreyImage $ fromFunction (size :. 1) $ \(i :. ~1) ->
            let !r = rgb ! (i :. 0)
                !g = rgb ! (i :. 1)
                !b = rgb ! (i :. 2)
            in word8 $ rgbToGrey r g b
      where
        size :. ~3 = extent rgb
    {-# INLINE safeConvert #-}

-- | Converts the colors to greyscale using the human eye colors perception.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Int
rgbToGrey r g b = (int r * 30 + int g * 59 + int b * 11) `quot` 100
{-# INLINE rgbToGrey #-}

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
