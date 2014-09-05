{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for grey-scale images.
module Vision.Image.Grey.Specialize () where

import Data.Int

import Vision.Histogram (Histogram, histogram, histogram2D, equalizeImage)
import Vision.Image.Grey.Type (Grey, GreyPixel)
import Vision.Image.Threshold (ThresholdType, otsu)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive (DIM1, DIM3, Rect, Size)

{-# SPECIALIZE histogram :: Maybe DIM1 -> Grey -> Histogram DIM1 Int32
                         ,  Maybe DIM1 -> Grey -> Histogram DIM1 Double
                         ,  Maybe DIM1 -> Grey -> Histogram DIM1 Float  #-}

{-# SPECIALIZE histogram2D :: DIM3 -> Grey -> Histogram DIM3 Int32
                           ,  DIM3 -> Grey -> Histogram DIM3 Double
                           ,  DIM3 -> Grey -> Histogram DIM3 Float  #-}

-- FIXME: GHC 7.8.2 fails to specialize
{-# SPECIALIZE equalizeImage :: Grey -> Grey #-}

{-# SPECIALIZE crop           :: Rect -> Grey -> Grey #-}
{-# SPECIALIZE resize         :: InterpolMethod -> Size -> Grey -> Grey #-}
{-# SPECIALIZE horizontalFlip :: Grey -> Grey #-}
{-# SPECIALIZE verticalFlip   :: Grey -> Grey #-}

{-# SPECIALIZE otsu           :: ThresholdType GreyPixel GreyPixel -> Grey
                              -> Grey #-}