{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for RGBA images.
module Vision.Image.RGBA.Specialize () where

import Data.Int

import Vision.Histogram (Histogram, histogram, histogram2D)
import Vision.Image.RGBA.Type (RGBA, RGBAPixel)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive (DIM4, DIM6, Rect, Size)

{-# SPECIALIZE histogram :: Maybe DIM4 -> RGBA -> Histogram DIM4 Int32
                         ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Double
                         ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Float  #-}

{-# SPECIALIZE histogram2D :: DIM6 -> RGBA -> Histogram DIM6 Int32
                           ,  DIM6 -> RGBA -> Histogram DIM6 Double
                           ,  DIM6 -> RGBA -> Histogram DIM6 Float  #-}

{-# SPECIALIZE crop           :: Rect -> RGBA -> RGBA #-}
{-# SPECIALIZE resize         :: InterpolMethod -> Size -> RGBA -> RGBA #-}
{-# SPECIALIZE horizontalFlip :: RGBA -> RGBA #-}
{-# SPECIALIZE verticalFlip   :: RGBA -> RGBA #-}
