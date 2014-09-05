{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for HSV images.
module Vision.Image.HSV.Specialize () where

import Data.Int

import Vision.Histogram (Histogram, histogram, histogram2D)
import Vision.Image.HSV.Type (HSV)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive (DIM3, DIM5, Rect, Size)

{-# SPECIALIZE histogram :: Maybe DIM3 -> HSV  -> Histogram DIM3 Int32
                         ,  Maybe DIM3 -> HSV  -> Histogram DIM3 Double
                         ,  Maybe DIM3 -> HSV  -> Histogram DIM3 Float  #-}

--                          ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Int32
--                          ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Double
--                          ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Float
--                          ,  Maybe DIM3 -> RGB  -> Histogram DIM3 Int32
--                          ,  Maybe DIM3 -> RGB  -> Histogram DIM3 Double
--                          ,  Maybe DIM3 -> RGB  -> Histogram DIM3 Float  #-}

{-# SPECIALIZE histogram2D :: DIM5 -> HSV  -> Histogram DIM5 Int32
                           ,  DIM5 -> HSV  -> Histogram DIM5 Double
                           ,  DIM5 -> HSV  -> Histogram DIM5 Float  #-}

--                            ,  DIM6 -> RGBA -> Histogram DIM6 Int32
--                            ,  DIM6 -> RGBA -> Histogram DIM6 Double
--                            ,  DIM6 -> RGBA -> Histogram DIM6 Float
--                            ,  DIM5 -> RGB  -> Histogram DIM5 Int32
--                            ,  DIM5 -> RGB  -> Histogram DIM5 Double
--                            ,  DIM5 -> RGB  -> Histogram DIM5 Float  #-}

{-# SPECIALIZE crop           :: Rect -> HSV -> HSV #-}
{-# SPECIALIZE resize         :: InterpolMethod -> Size -> HSV -> HSV #-}
{-# SPECIALIZE horizontalFlip :: HSV -> HSV #-}
{-# SPECIALIZE verticalFlip   :: HSV -> HSV #-}
