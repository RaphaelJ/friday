{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, PatternGuards
           , TypeFamilies, UndecidableInstances #-}
-- | Provides an interface for images which can be masked.
--
-- A masked image is an image on which certain pixels are not defined nor
-- accessible. Masked images are useful when you want to ignore some pixels of
-- the image, such as background pixels.
--
-- It\'s build around the 'MaskedImage' type class. Every 'Image' is also an
-- instance of 'MaskedImage' (they are masked images on which no pixel is
-- masked).
--
-- As some function names conflict with those found for traditional images,
-- it's recommended to import this module with a qualifier.
module Vision.Image.Mask (
      MaskedImage (..), MaskedChannel, DelayedMask (..), map
    ) where

import Control.Applicative ((<$>))
import Data.Vector.Storable (Vector, unfoldr)
import Foreign.Storable (Storable)
import Prelude hiding (map)

import Vision.Image.Type (Pixel (..), FromFunction (..), Manifest, Delayed)
import qualified Vision.Image.Type as I
import Vision.Primitive (
      Shape, Point, Size, fromLinearIndex, toLinearIndex, shapeLength
    )

-- Classes ---------------------------------------------------------------------

-- | Provides an abstraction for images which are not defined for each of their
-- pixels. The interface is similar to 'Image' except that indexing functions
-- don't always return.
--
-- Minimal definition is 'shape' and ('index' or 'linearIndex').
class Pixel (MaskedPixel i) => MaskedImage i where
    type MaskedPixel i

    shape :: i -> Size

    -- | Returns the pixel\'s value at 'Z :. y, :. x'.
    index :: i -> Point -> Maybe (MaskedPixel i)
    index img = (img `linearIndex`) . toLinearIndex (shape img)
    {-# INLINE index #-}

    -- | Returns the pixel\'s value as if the image was a single dimension
    -- vector (row-major representation).
    linearIndex :: i -> Int -> Maybe (MaskedPixel i)
    linearIndex img = (img `index`) . fromLinearIndex (shape img)
    {-# INLINE linearIndex #-}

    -- | Returns the non-masked values of the image.
    values :: i -> Vector (MaskedPixel i)
    values img =
        unfoldr step 0
      where
        !n = shapeLength (shape img)

        step !i | i >= n                        = Nothing
                | Just p <- img `linearIndex` i = Just (p, i)
                | otherwise                     = step (i+1)
    {-# INLINE values #-}

type MaskedChannel i = PixelChannel (MaskedPixel i)

-- Delayed masks ---------------------------------------------------------------

data DelayedMask p = DelayedMask {
      delayedMaskSize :: !Size
    , delayedMaskFun  :: (Point -> Maybe p)
    }

instance Pixel p => MaskedImage (DelayedMask p) where
    type MaskedPixel (DelayedMask p) = p

    shape = delayedMaskSize
    {-# INLINE shape #-}

    index = delayedMaskFun
    {-# INLINE index #-}

instance Pixel p => FromFunction (DelayedMask p) where
    type FromFunctionPixel (DelayedMask p) = Maybe p

    fromFunction = DelayedMask
    {-# INLINE fromFunction #-}

-- Instances -------------------------------------------------------------------

instance (Pixel p, Storable p) => MaskedImage (Manifest p) where
    type MaskedPixel (Manifest p) = p

    shape             = I.shape
    {-# INLINE shape #-}

    index       img   = Just . I.index img
    {-# INLINE index #-}

    linearIndex img   = Just . I.linearIndex img
    {-# INLINE linearIndex #-}

    values            = I.vector
    {-# INLINE values #-}

instance Pixel p => MaskedImage (Delayed p) where
    type MaskedPixel (Delayed p) = p

    shape             = I.shape
    {-# INLINE shape #-}

    index       img   = Just . I.index img
    {-# INLINE index #-}

    linearIndex img   = Just . I.linearIndex img
    {-# INLINE linearIndex #-}

    values            = I.vector
    {-# INLINE values #-}

-- Conversions -----------------------------------------------------------------

map :: (MaskedImage i1, FromFunction i2, Maybe p ~ FromFunctionPixel i2)
    => (MaskedPixel i1 -> p) -> i1 -> i2
map f img = fromFunction (shape img) (\pt -> f <$> img `index` pt)
{-# INLINE map #-}