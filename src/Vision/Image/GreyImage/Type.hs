{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses
           , TypeFamilies, UndecidableInstances #-}
module Vision.Image.GreyImage.Type (GreyImage (..), GreyPixel (..)) where

import Data.Array.Repa (Array, D, DIM3, Source, (:.) (..), (!))
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

    fromFunction size pixel = GreyImage $
        R.fromFunction (size :. 1) $ \(dim2 :. ~0) ->
            let GreyPixel pix = pixel dim2
            in pix
    {-# INLINE fromFunction #-}

instance Interpolable GreyImage where
    interpol _ f (GreyPixel a) (GreyPixel b) = GreyPixel $ f a b
    {-# INLINE interpol #-}

instance Source r (Channel GreyImage) => Eq (GreyImage r) where
    a == b = toRepa a == toRepa b

instance Show (Array r DIM3 (Channel GreyImage)) => Show (GreyImage r) where
    show = show . toRepa
