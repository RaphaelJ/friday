-- | Provides an interface for images which can be masked.
import Vision.Image.Mask (MaskedImage (..)) where

import Data.Vector.Storable (Vector, (!), create, enumFromN, forM_, generate)

import Visio.Image.Type (Image, Pixel (..))
import Vision.Primitive (
      Z (..), (:.) (..), Shape, Point, Size
    , ix2, fromLinearIndex, toLinearIndex, shapeLength
    )

class Pixel (MaskedPixel i) => MaskedImage i where
    type MaskedPixel i

    shape :: i -> Size

    -- | Returns the pixel\'s value at 'Z :. y :. x'.
    index :: i -> Point -> Maybe (MaskedPixel i)
    index img = (img `linearIndex`) . toLinearIndex (shape img)
    {-# INLINE index #-}

    -- | Returns the pixel\'s value as if the image was a single dimension
    -- vector (row-major representation).
    linearIndex :: i -> Int -> Maybe (MaskedPixel i)
    linearIndex img = (img `index`) . fromLinearIndex (shape img)
    {-# INLINE linearIndex #-}

    -- | Returns every pixel values as if the image was a single dimension
    -- vector (row-major representation).
    vector :: i -> Vector (Maybe (MaskedPixel i))
    vector img = generate (shapeLength $ shape img) (img `linearIndex`)
    {-# INLINE vector #-}

    -- | Returns the non-masked values of the image.
    -- > values = filter isJust . vector
    values :: i -> Vector (MaskedPixel i)
    values = filter isJust . vector
    {-# INLINE values #-}

instance Image i => MaskedImage where
    type MaskedPixel i = I.ImagePixel i

    shape = I.shape
    {-# INLINE shape #-}

    index img = Just . I.index img
    {-# INLINE index #-}

    linearIndex img = Just . I.linearIndex img
    {-# INLINE linearIndex #-}

    vector = V.map . I.vector
    {-# INLINE vector #-}

    values = I.vector
    {-# INLINE values #-}

type MaskedChannel i = PixelChannel (MaskedPixel i)

data (Image i, Image mask, ImagePixel mask ~ Bool) => Masked = Masked {
      maskedImg  :: i
    , maskedMask :: mask
    } (deriving Eq, Ord, Show)
