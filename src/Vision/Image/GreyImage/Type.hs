{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Vision.Image.GreyImage.Type (GreyImage (..), GreyPixel (..)) where

import Data.Array.Repa (Array, D, DIM3, (:.) (..), (!))
import qualified Data.Array.Repa as R
import Data.Word

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Interpolate (Interpolable (..))

newtype GreyImage r = GreyImage { unGreyImage :: Array r DIM3 Word8 }

newtype GreyPixel = GreyPixel Word8

instance Image GreyImage where
    type Pixel   GreyImage = GreyPixel
    type Channel GreyImage = Word8

    nChannels _ = 1
    {-# INLINE nChannels #-}

    toRepa   = unGreyImage
    {-# INLINE toRepa #-}

    fromRepa = GreyImage
    {-# INLINE fromRepa #-}

    GreyImage img `getPixel` sh = GreyPixel $ img ! (sh :. 0)
    {-# INLINE getPixel #-}

instance FromFunction GreyImage where
    type FunctionRepr GreyImage = D

    fromFunction size f = GreyImage $
        R.fromFunction (size :. 1) $ \(coords :. ~0) ->
            let GreyPixel pix = f coords
            in pix
    {-# INLINE fromFunction #-}

instance Interpolable GreyImage where
    interpol2 _ f (GreyPixel a) (GreyPixel b) = GreyPixel $ f a b
    {-# INLINE interpol2 #-}

    interpol4 _ f (GreyPixel a) (GreyPixel b) (GreyPixel c) (GreyPixel d) =
        GreyPixel $ f a b c d
    {-# INLINE interpol4 #-}
