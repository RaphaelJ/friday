-- | This module define a set of primitive function to deal with images.
module Vision.Image.Function (nChannels, pixel) where

import Vision.Image.Mask (MaskedImage, MaskedPixel)
import Vision.Image.Type (pixNChannels)

-- | Returns the number of channels of an image.
nChannels :: MaskedImage i => i -> Int
nChannels img = pixNChannels (pixel img)
{-# INLINE nChannels #-}

-- | Returns an 'undefined' instance of a pixel of the image. This is sometime
-- useful to satisfy the type checker as in a call to 'pixNChannels' :
--
-- > nChannels img = pixNChannels (pixel img)
pixel :: MaskedImage i => i -> MaskedPixel i
pixel _ = undefined
