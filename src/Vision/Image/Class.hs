{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
           , TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.Class (Image (..), FromFunction (..)) where

import Data.Array.Repa (Array, DIM2, DIM3, Source)

class Image i where
    -- | Determines the number of channels and the type of each pixel of the
    -- image.
    type Pixel i

    -- | Determines the type of each channel of the pixel.
    type Channel i

    -- | Determines the number of channels of each pixel.
    -- This must be equal to the size of the last dimension of the array.
    nChannels :: i r -> Int

    -- | Unboxes the internal Repa representation of the image.
    -- Each image is represented as a 3-dimensional array with
    -- Z :. @row@ :. @column@ :. @color channel@
    toRepa   :: i r -> Array r DIM3 (Channel i)

    -- | Boxes the internal Repa representation of the image.
    fromRepa :: Array r DIM3 (Channel i) -> i r

    -- | Returns the value of every channel at the given coordinates.
    getPixel :: Source r (Channel i) => i r -> DIM2 -> Pixel i

-- | Defines how an image can be generated from a function.
class Image i => FromFunction i where
    type FunctionRepr i

    fromFunction :: DIM2 -> (DIM2 -> Pixel i) -> i (FunctionRepr i)
