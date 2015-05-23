{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @SPECIALIZE@ pragma declarations for grey-scale images.
module Vision.Image.Grey.Specialize () where

import Data.Int

import Vision.Histogram ({- Histogram, histogram, histogram2D, -}equalizeImage)
import Vision.Image.Filter (
      DerivativeType, dilate, erode, blur, gaussianBlur, scharr, sobel, mean
    )
import Vision.Image.Grey.Type (Grey{-, GreyPixel-})
-- import Vision.Image.Threshold (
--       ThresholdType, AdaptiveThresholdKernel, adaptiveThreshold, otsu, scw
--     )
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Image.Type (Manifest)
import Vision.Primitive ({-DIM1, DIM3, -}Rect, Size)

{-# SPECIALIZE NOINLINE blur           :: Int -> Grey -> Grey #-}

{-# SPECIALIZE NOINLINE crop           :: Rect -> Grey -> Grey #-}

{-# SPECIALIZE NOINLINE dilate         :: Int -> Grey -> Grey #-}

{-# SPECIALIZE NOINLINE equalizeImage  :: Grey -> Grey #-}

{-# SPECIALIZE NOINLINE erode          :: Int -> Grey -> Grey #-}

{-# SPECIALIZE NOINLINE gaussianBlur   :: Int -> Maybe Double -> Grey -> Grey
                                       ,  Int -> Maybe Float  -> Grey -> Grey
                                       #-}

{-# SPECIALIZE NOINLINE horizontalFlip :: Grey -> Grey #-}

{-# SPECIALIZE NOINLINE mean           :: Size -> Grey -> Manifest Double
                                       ,  Size -> Grey -> Manifest Float #-}

{-# SPECIALIZE NOINLINE resize         :: InterpolMethod -> Size -> Grey -> Grey #-}

{-# SPECIALIZE NOINLINE scharr         :: DerivativeType -> Grey -> Grey
                                       ,  DerivativeType -> Grey
                                                         -> Manifest Int16 #-}

{-# SPECIALIZE NOINLINE scharr         :: DerivativeType -> Grey
                                                         -> Manifest Int32
                                       ,  DerivativeType -> Grey
                                                         -> Manifest Int #-}

{-# SPECIALIZE NOINLINE sobel          :: Int -> DerivativeType -> Grey -> Grey
                                       ,  Int -> DerivativeType -> Grey
                                              -> Manifest Int16
                                       ,  Int -> DerivativeType -> Grey
                                              -> Manifest Int32
                                       ,  Int -> DerivativeType -> Grey
                                              -> Manifest Int #-}
{-# SPECIALIZE NOINLINE verticalFlip   :: Grey -> Grey #-}

-- FIXME: GHC 7.10 fails to specialize the following rules :
--
-- {-# SPECIALIZE histogram :: Maybe DIM1 -> Grey -> Histogram DIM1 Int32
--                          ,  Maybe DIM1 -> Grey -> Histogram DIM1 Double
--                          ,  Maybe DIM1 -> Grey -> Histogram DIM1 Float  #-}
-- 
-- {-# SPECIALIZE histogram2D :: DIM3 -> Grey -> Histogram DIM3 Int32
--                            ,  DIM3 -> Grey -> Histogram DIM3 Double
--                            ,  DIM3 -> Grey -> Histogram DIM3 Float  #-}
--
-- {-# SPECIALIZE adaptiveThreshold :: AdaptiveThresholdKernel Int -> Int
--                                  -> GreyPixel
--                                  -> ThresholdType GreyPixel GreyPixel
--                                  -> Grey -> Grey #-}
-- {-# SPECIALIZE adaptiveThreshold :: AdaptiveThresholdKernel Int16 -> Int
--                                  -> GreyPixel
--                                  -> ThresholdType GreyPixel GreyPixel
--                                  -> Grey -> Grey  #-}
-- {-# SPECIALIZE adaptiveThreshold :: AdaptiveThresholdKernel Int32 -> Int
--                                  -> GreyPixel
--                                  -> ThresholdType GreyPixel GreyPixel
--                                  -> Grey -> Grey  #-}
-- {-# SPECIALIZE adaptiveThreshold :: AdaptiveThresholdKernel Double -> Int
--                                  -> GreyPixel
--                                  -> ThresholdType GreyPixel GreyPixel
--                                  -> Grey -> Grey  #-}
-- {-# SPECIALIZE adaptiveThreshold :: AdaptiveThresholdKernel Float -> Int
--                                  -> GreyPixel
--                                  -> ThresholdType GreyPixel GreyPixel
--                                  -> Grey -> Grey #-}
--
-- {-# SPECIALIZE otsu              :: ThresholdType GreyPixel GreyPixel -> Grey
--                                  -> Grey #-}
--
-- {-# SPECIALIZE scw               :: Size -> Size -> Double
--                                  -> ThresholdType GreyPixel GreyPixel -> Grey
--                                  -> Grey #-}
-- {-# SPECIALIZE scw               :: Size -> Size -> Float
--                                  -> ThresholdType GreyPixel GreyPixel -> Grey
--                                  -> Grey #-}
