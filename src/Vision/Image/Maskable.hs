-- | Provides an interface for images which can be masked.
--
-- A masked image is an image on which certain pixels are not defined nor
-- accessible. Masked images are useful when you want to ignore some pixels of
-- the image, such as background pixels.
--
-- It\'s build around the 'MaskableImage' type class. Every 'Image' is also an
-- instance of 'MaskableImage' (they are masked images on which no pixel is 
-- masked).
--
-- As some function names conflict with those found for traditional images,
-- it's recommended to import this module with a qualifier.
import Vision.Image.Maskable (MaskableImage (..))
    where

import Data.Vector.Storable (Vector, (!), create, enumFromN, forM_, generate)

import Visio.Image.Type (Image, Pixel (..))
import Vision.Primitive (
      Z (..), (:.) (..), Shape, Point, Size
    , ix2, fromLinearIndex, toLinearIndex, shapeLength
    )

-- Classes ---------------------------------------------------------------------

data Masked i mask = {
    
    }

-- | Provides an abstraction over the internal representation of a masked image.
-- The interface is very similar to 'Image', only returning 'Maybe' values
-- when examining a pixel.
--
-- Minimal definition is 'shape' and ('index' or 'linearIndex').
class (Image (MaskedImage i)) => Maskable i where
    type MaskedImage i

    image :: i -> MaskedImage i

    isMasked :: i -> Point -> Bool

    -- | Returns the non-masked values of the image.
    values :: i -> Vector (MaskedPixel i)
    values = filter isJust . I.vector i
    {-# INLINE values #-}

type MaskedPixel i = ImagePixel (MaskedImage i)

shape :: MaskableImage i => i -> Size 
shape = I.shape . image

-- | Returns the pixel\'s value at 'Z :. y :. x', if not masked.
index :: i -> Point -> Maybe (MaskedPixel i)
index img = (img `linearIndex`) . toLinearIndex (shape img)
{-# INLINE index #-}

-- | Returns the pixel\'s value, if not masked, as if the image was a single
-- dimension vector (row-major representation).
linearIndex :: i -> Int -> Maybe (MaskedPixel i)
linearIndex img = (img `index`) . fromLinearIndex (shape img)
{-# INLINE linearIndex #-}

-- | Returns the non-masked values of the image.
-- > values = filter isJust . vector
values :: i -> Vector (MaskedPixel i)
values = filter isJust . vector
{-# INLINE values #-}

instance Image i => Maskable i where
    type MaskedImage i = i
    type OutputPixel i = ImagePixel i
    
    getPixel = I.getPixel

    image = id
    {-# INLINE image #-}

    isMasked _ = False
    {-# INLINE isMasked #-}

    values = I.vector
    {-# INLINE values #-}

type MaskedChannel i = PixelChannel (MaskedPixel i)

type Mask = Manifest Bool

data Masked i mask = Masked {
      maskedImg  :: !i
    , maskedMask :: !mask
    } (deriving Eq, Ord, Show)
    
`getPixel`

instance Image i => Image (Masked i) where
    type ImagePixel (Masked i) = Maybe (ImagePixel i)
    
    shape Masked = 

    index img = (img `linearIndex`) . toLinearIndex (shape img)
    {-# INLINE index #-}

    -- | Returns the pixel\'s value as if the image was a single dimension
    -- vector (row-major representation).
    linearIndex :: i -> Int -> ImagePixel i
    linearIndex img = (img `index`) . fromLinearIndex (shape img)
    {-# INLINE linearIndex #-}

    -- | Returns every pixel values as if the image was a single dimension
    -- vector (row-major representation).
    vector :: i -> Vector (ImagePixel i)
    vector img = generate (shapeLength $ shape img) (img `linearIndex`)
    {-# INLINE vector #-}
    

instance (Image i, Image mask, ImagePixel mask ~ Bool)
    => MaskableImage (Masked i mask) where
    type MaskedPixel i = I.ImagePixel i

    shape = I.shape . maskedImg
    {-# INLINE shape #-}

    Masked img mask `index` !sh
        | mask `I.index` sh = Just $! img `I.index` sh
        | otherwise         = Nothing
    {-# INLINE index #-}

    Masked img mask `index` !i
        | mask `I.linearIndex` i = Just $! img `I.linearIndex` sh
        | otherwise              = Nothing
    linearIndex img = Just . I.linearIndex img
    {-# INLINE linearIndex #-}

    vector (Masked img mask) =
        V.zipWith f (I.vector img) (I.vector mask)
      where
        f v True  = Just v
        f _ False = Nothing
    {-# INLINE vector #-}

    values (Masked img mask) =
        V.ifilter f (I.vector img)
      where
        f i v | mask `I.linearIndex` i = Just v
              | otherwise              = Nothing
    {-# INLINE values #-}
