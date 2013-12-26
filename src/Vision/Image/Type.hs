{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.Type (
    -- * Classes
      Pixel (..), Image (..), ImageChannel, FromFunction (..)
    -- * Manifest images
    , Manifest (..)
    -- * Delayed images
    , Delayed (..)
    -- * Functions
    , nChannels, map, delay, compute, pixel
    ) where

import Data.Convertible (Convertible (..), convert)
import Data.Vector.Storable (Vector, (!), create, enumFromN, forM_, generate)
import Data.Vector.Storable.Mutable (new, write)
import Foreign.Storable (Storable)
import Prelude hiding (map)

import Vision.Primitive (
      Z (..), (:.) (..), Shape, Point, Size
    , ix2, fromLinearIndex, toLinearIndex, shapeLength
    )

-- Classes ---------------------------------------------------------------------

-- | Determines the number of channels and the type of each pixel of the image
-- and how images are represented.
class (Storable p, Storable (PixelChannel p)) => Pixel p where
    type PixelChannel p

    -- | Returns the number of channels of the pixel.
    -- Shouldn't consume 'p' (could be 'undefined').
    pixNChannels :: p -> Int

    pixIndex :: p -> Int -> PixelChannel p

-- | Provides an abstraction over the internal representation of the image.
-- Origin of images is located in the lower left corner.
-- Minimal definition is 'shape' and ('index' or 'linearIndex').
class Pixel (ImagePixel i) => Image i where
    type ImagePixel i

    shape :: i -> Size

    -- | Returns the pixel\'s value at 'Z :. y :. x'.
    index :: i -> Point -> ImagePixel i
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

type ImageChannel i = PixelChannel (ImagePixel i)

-- | Provides a way to construct an image from a function.
-- Minimal definition is 'fromFunction' or 'fromFunctionLine', the second one
-- being faster with some transformations and representations by memorising some
-- line invariants.
class (Pixel (ImagePixel i), Image i) => FromFunction i where
    -- | Calls the given function for each pixel of the constructed image.
    fromFunction :: Size -> (Point -> ImagePixel i) -> i
    fromFunction size f =
        fromFunctionLine size (const ()) (\_ pt -> f pt)
    {-# INLINE fromFunction #-}

    -- | Calls the first function at each line of the image then calls the
    -- second function for each pixel of the constructed image, giving the
    -- value which has been computed at each line.
    fromFunctionLine :: Size -> (Int -> a) -> (a -> Point -> ImagePixel i) -> i
    fromFunctionLine size line f =
        fromFunction size (\pt@(Z :. y :. _) -> f (line y) pt)
    {-# INLINE fromFunctionLine #-}

-- Manifest images -------------------------------------------------------------

-- | Stores the image\'s content in a 'Vector'.
data Manifest p = Manifest {
      manifestSize   :: !Size
    , manifestVector :: !(Vector p)
    } deriving (Eq, Show)

instance (Pixel p, Storable p) => Image (Manifest p) where
    type ImagePixel (Manifest p) = p

    shape = manifestSize
    {-# INLINE shape #-}

    Manifest _ vec `linearIndex` ix = vec ! ix
    {-# INLINE linearIndex #-}

    vector = manifestVector
    {-# INLINE vector #-}

instance (Pixel p, Storable p) => FromFunction (Manifest p) where
    fromFunctionLine size@(Z :. h :. w) line f = Manifest size $ create $ do
        -- Note: create is faster than unfoldrN.
        arr <- new (h * w)

        forM_ (enumFromN 0 h) $ \y -> do
            let !lineVal    = line y
                !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                let !offset = lineOffset + x
                    !val    = f lineVal (ix2 y x)
                write arr offset val

        return arr
    {-# INLINE fromFunctionLine #-}

-- Delayed images --------------------------------------------------------------

-- | A delayed image is an image which is constructed using a function.
-- Usually, a delayed image maps each of its pixels over another image.
-- Delayed images are useful by avoiding intermediate images in a
-- transformation pipeline of images or by avoiding the computation of the whole
-- resulting image when only a portion of its pixels will be accessed.
data Delayed p = Delayed {
      delayedSize :: !Size
    , delayedFun  :: (Point -> p)
    }

instance Pixel p => Image (Delayed p) where
    type ImagePixel (Delayed p) = p

    shape = delayedSize
    {-# INLINE shape #-}

    index = delayedFun
    {-# INLINE index #-}

instance Pixel p => FromFunction (Delayed p) where
    fromFunction = Delayed
    {-# INLINE fromFunction #-}

-- Functions -------------------------------------------------------------------

-- | Returns the number of channels of an image.
nChannels :: Image i => i -> Int
nChannels img = pixNChannels (pixel img)
{-# INLINE nChannels #-}

map :: (Image i1, FromFunction i2)
    => (ImagePixel i1 -> ImagePixel i2) -> i1 -> i2
map f img = fromFunction (shape img) (f . (img `index`))
{-# INLINE map #-}

-- | Delays an image in its delayed representation.
delay :: Image i => i -> Delayed (ImagePixel i)
delay = map id
{-# INLINE delay #-}

-- | Computes the value of an image into a manifest representation.
compute :: (Image i, Storable (ImagePixel i)) => i -> Manifest (ImagePixel i)
compute = map id
{-# INLINE compute #-}

-- | Returns an 'undefined' instance of a pixel of the image. This is sometime
-- useful to satisfy the type checker as in a call to 'pixNChannels' :
-- 
-- > nChannels img = pixNChannels (pixel img)
pixel :: Image i => i -> ImagePixel i
pixel _ = undefined

-- Conversion ------------------------------------------------------------------

instance (Pixel p1, Pixel p2, Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Manifest p1) (Manifest p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Pixel p1, Pixel p2, Convertible p1 p2)
    => Convertible (Delayed p1) (Delayed p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Pixel p1, Pixel p2, Storable p2, Convertible p1 p2)
    => Convertible (Delayed p1) (Manifest p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Pixel p1, Pixel p2, Storable p1, Convertible p1 p2)
    => Convertible (Manifest p1) (Delayed  p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}
