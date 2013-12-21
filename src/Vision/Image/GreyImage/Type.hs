{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Vision.Image.GreyImage.Type (
      GreyPixel (..), GreyImage, GreyDelayed
    ) where

import Data.Word
import Foreign.Storable (Storable)

import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Primitive (Rect, Size)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Image.Type (Pixel (..), Manifest, Delayed)

newtype GreyPixel = GreyPixel Word8
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Storable)

type GreyImage = Manifest GreyPixel

type GreyDelayed = Delayed GreyPixel

instance Pixel GreyPixel where
    type PixelChannel GreyPixel = Word8

    nChannels _ = 1
    {-# INLINE nChannels #-}

instance Interpolable GreyPixel where
    interpol f (GreyPixel a) (GreyPixel b) = GreyPixel $ f a b
    {-# INLINE interpol #-}

{-# SPECIALIZE crop :: GreyImage -> Rect -> GreyImage #-}
{-# SPECIALIZE resize :: GreyImage -> InterpolMethod -> Size -> GreyImage #-}
{-# SPECIALIZE horizontalFlip :: GreyImage -> GreyImage #-}
{-# SPECIALIZE verticalFlip :: GreyImage -> GreyImage #-}
