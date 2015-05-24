{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for RGBA images.
module Vision.Image.RGBA.Specialize () where

-- import Data.Int

-- import Vision.Histogram (Histogram, histogram, histogram2D)
import Vision.Image.RGBA.Type (RGBA)
import Vision.Image.Transform (
      TruncateInteger, NearestNeighbor, Bilinear
    , crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive ({-DIM4, DIM6, -}Rect, Size)

{-# SPECIALIZE NOINLINE crop           :: Rect -> RGBA -> RGBA #-}

{-# SPECIALIZE NOINLINE horizontalFlip :: RGBA -> RGBA #-}

{-# SPECIALIZE NOINLINE resize         :: TruncateInteger -> Size -> RGBA
                                                          -> RGBA
                                       ,  NearestNeighbor -> Size -> RGBA
                                                          -> RGBA
                                       ,  Bilinear -> Size -> RGBA -> RGBA #-}

{-# SPECIALIZE NOINLINE verticalFlip   :: RGBA -> RGBA #-}

-- FIXME: GHC 7.10 fails to specialize the following rules :
--
-- {-# SPECIALIZE histogram :: Maybe DIM4 -> RGBA -> Histogram DIM4 Int32
--                          ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Double
--                          ,  Maybe DIM4 -> RGBA -> Histogram DIM4 Float  #-}
-- 
-- {-# SPECIALIZE histogram2D :: DIM6 -> RGBA -> Histogram DIM6 Int32
--                            ,  DIM6 -> RGBA -> Histogram DIM6 Double
--                            ,  DIM6 -> RGBA -> Histogram DIM6 Float  #-}