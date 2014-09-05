{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for RGB images.
module Vision.Image.RGB.Specialize () where

import Data.Int

import Vision.Histogram (Histogram, histogram, histogram2D)
import Vision.Image.RGB.Type (RGB)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive (DIM3, DIM5, Rect, Size)

{-# SPECIALIZE histogram :: Maybe DIM3 -> RGB  -> Histogram DIM3 Int32
                         ,  Maybe DIM3 -> RGB  -> Histogram DIM3 Double
                         ,  Maybe DIM3 -> RGB  -> Histogram DIM3 Float  #-}

{-# SPECIALIZE histogram2D :: DIM5 -> RGB  -> Histogram DIM5 Int32
                           ,  DIM5 -> RGB  -> Histogram DIM5 Double
                           ,  DIM5 -> RGB  -> Histogram DIM5 Float  #-}

{-# SPECIALIZE crop           :: Rect -> RGB -> RGB #-}
{-# SPECIALIZE resize         :: InterpolMethod -> Size -> RGB -> RGB #-}
{-# SPECIALIZE horizontalFlip :: RGB -> RGB #-}
{-# SPECIALIZE verticalFlip   :: RGB -> RGB #-}
