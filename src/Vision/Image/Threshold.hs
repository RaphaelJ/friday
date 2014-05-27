module Vision.Image.Threshold (threshold, adaptiveThreshold) where

import Vision.Image.Filter (gaussianBlur)
import Vision.Image.Type (FunctorImage)
import qualified Vision.Image.Type as I

data ThresholdType src res where
    BinaryThreshold :: ImagePixel res -- ^ Pixel value if the predicate matches.
                    -> ImagePixel res -- ^ Pixel value if the predicate doesn't
                                      -- match.
                    -> ThresholdType src res
    -- | Replaces the pixel by the given value only when the predicate matches.
    Truncate        :: ImagePixel src
                    -> ThresholdType src src

-- | Applies the given predicate and threshold policy on the image.
threshold :: FunctorImage src res
          => src -> (ImagePixel src -> Bool) -> ThresholdType src res -> res
threshold !img !cond !(BinaryThreshold ifTrue ifFalse) =
    I.map (\pix -> if cond then ifTrue else ifFalse) img
threshold !img !cond !(Truncate        ifTrue)         =
    I.map (\pix -> if cond then ifTrue else pix) img
{-# INLINE threshold #-}

-- data ThresholdKernel acc where
--     MeanKernel     :: Num ThresholdKernel
--     GaussianKernel :: Floating acc, RealFrac acc => GaussianKernel
-- 
-- adaptiveThreshold :: (Storable acc)
--                   => ThresholdKernel acc
--                   -> AdaptiveThresholdType -> acc -> ThresholdType
--                   -> SeparableFilter src acc res
-- adaptiveMeanThreshold kernelType radius thres =
--     case kernelType of
--         MeanThreshold -> 
-- 
-- {-# INLINE adaptiveThreshold #-}