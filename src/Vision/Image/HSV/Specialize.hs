{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for HSV images.
module Vision.Image.HSV.Specialize () where

-- import Data.Int

-- import Vision.Histogram (Histogram, histogram, histogram2D)
import Vision.Image.HSV.Type (HSV)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive ({-DIM3, DIM5, -}Rect, Size)

{-# SPECIALIZE crop           :: Rect -> HSV -> HSV #-}

{-# SPECIALIZE horizontalFlip :: HSV -> HSV #-}

{-# SPECIALIZE resize         :: InterpolMethod -> Size -> HSV -> HSV #-}

{-# SPECIALIZE verticalFlip   :: HSV -> HSV #-}


-- FIXME: GHC 7.10 fails to specialize the following rules :
--
-- {-# SPECIALIZE histogram :: Maybe DIM3 -> HSV  -> Histogram DIM3 Int32
--                          ,  Maybe DIM3 -> HSV  -> Histogram DIM3 Double
--                          ,  Maybe DIM3 -> HSV  -> Histogram DIM3 Float  #-}

-- {-# SPECIALIZE histogram2D :: DIM5 -> HSV  -> Histogram DIM5 Int32
--                            ,  DIM5 -> HSV  -> Histogram DIM5 Double
--                            ,  DIM5 -> HSV  -> Histogram DIM5 Float  #-}
