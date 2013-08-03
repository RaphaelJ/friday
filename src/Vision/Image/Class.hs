{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
           , TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vision.Image.Class (Image (..), FromFunction (..)) where

import Data.Array.Repa (Array, DIM1, DIM2, DIM3, Source, (:.) (..))

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
-- Minimal definition is 'fromFunction' or 'fromFunctionLine'.
class Image i => FromFunction i where
    type FunctionRepr i

    -- | Calls the given function for each pixel of the constructed image.
    fromFunction :: DIM2 -> (DIM2 -> Pixel i) -> i (FunctionRepr i)
    fromFunction size pixel = fromFunctionLine size (const ())
                                                    (\_ dim2 -> pixel dim2)
    {-# INLINE fromFunction #-}

    -- | Calls the first function at each line of the image then calls the
    -- second function for each pixel of the constructed image, giving the
    -- value which has been computed at each line.
    fromFunctionLine :: DIM2 -> (DIM1 -> a) -> (a -> DIM2 -> Pixel i)
                     -> i (FunctionRepr i)
    fromFunctionLine size line pixel =
        fromFunction size (\dim2@(dim1 :. _) -> pixel (line dim1) dim2)
    {-# INLINE fromFunctionLine #-}
