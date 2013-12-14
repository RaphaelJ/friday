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
    , delay, compute
    ) where

import Data.Convertible (Convertible (..), convert)
import Data.Vector.Storable (Vector, (!), create, enumFromN, forM_)
import Data.Vector.Storable.Mutable (new, write)
import Foreign.Storable (Storable)

import Vision.Image.Primitive (Point (..), Size (..))

-- Classes ---------------------------------------------------------------------

-- | Determines the number of channels and the type of each pixel of the image
-- and how images are represented.
class Pixel p where
    type PixelChannel p

    nChannels :: p -> Int

-- | Provides an abstraction over the internal representation of the image.
-- Origin of images is located in the lower left corner.
class Pixel (ImagePixel i) => Image i where
    type ImagePixel i

    getSize :: i -> Size

    getPixel :: i -> Point -> ImagePixel i

type ImageChannel i = PixelChannel (ImagePixel i)

-- | Provides a way to construct an image from a function.
-- Minimal definition is 'fromFunction' or 'fromFunctionLine', the second one
-- being faster with some transformations and representations by memorising some
-- line invariants.
class (Pixel (ImagePixel i), Image i) => FromFunction i where
    -- | Calls the given function for each pixel of the constructed image.
    fromFunction :: Size -> (Point -> ImagePixel i) -> i
    fromFunction size pixel =
        fromFunctionLine size (const ()) (\_ pt -> pixel pt)
    {-# INLINE fromFunction #-}

    -- | Calls the first function at each line of the image then calls the
    -- second function for each pixel of the constructed image, giving the
    -- value which has been computed at each line.
    fromFunctionLine :: Size -> (Int -> a) -> (a -> Point -> ImagePixel i) -> i
    fromFunctionLine size line pixel =
        fromFunction size (\pt@(Point _ y) -> pixel (line y) pt)
    {-# INLINE fromFunctionLine #-}

-- Manifest images -------------------------------------------------------------

-- | Stores the image\'s content in a 'Vector'.
data Manifest p = Manifest {
      manifestSize   :: !Size
    , manifestVector :: !(Vector p)
    } deriving (Eq, Show)

instance (Pixel p, Storable p) => Image (Manifest p) where
    type ImagePixel (Manifest p) = p

    getSize = manifestSize
    {-# INLINE getSize #-}

    Manifest (Size w _) vec `getPixel` Point x y = vec ! (y * w + x)
    {-# INLINE getPixel #-}

instance (Pixel p, Storable p) => FromFunction (Manifest p) where
    fromFunctionLine size@(Size w h) line pixel = Manifest size $ create $ do
        arr <- new (h * w)

        forM_ (enumFromN 0 h) $ \y -> do
            let !lineVal    = line y
                !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                let !offset = lineOffset + x
                    !val    = pixel lineVal (Point x y)
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

    getSize = delayedSize
    {-# INLINE getSize #-}

    getPixel = delayedFun
    {-# INLINE getPixel #-}

instance Pixel p => FromFunction (Delayed p) where
    fromFunction = Delayed
    {-# INLINE fromFunction #-}

-- Conversion ------------------------------------------------------------------

instance (Pixel p1, Pixel p2, Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Manifest p1) (Manifest p2) where
    safeConvert img = Right $
        fromFunction (getSize img) (convert . (img `getPixel`))
    {-# INLINE safeConvert #-}

instance (Pixel p1, Pixel p2, Convertible p1 p2)
    => Convertible (Delayed p1) (Delayed p2) where
    safeConvert img = Right $
        fromFunction (getSize img) (convert . (img `getPixel`))
    {-# INLINE safeConvert #-}

instance (Pixel p1, Pixel p2, Storable p2, Convertible p1 p2)
    => Convertible (Delayed p1) (Manifest p2) where
    safeConvert img = Right $
        fromFunction (getSize img) (convert . (img `getPixel`))
    {-# INLINE safeConvert #-}

instance (Pixel p1, Pixel p2, Storable p1, Convertible p1 p2)
    => Convertible (Manifest p1) (Delayed  p2) where
    safeConvert img = Right $
        fromFunction (getSize img) (convert . (img `getPixel`))
    {-# INLINE safeConvert #-}

-- Functions -------------------------------------------------------------------

-- | Delays an image in its delayed representation.
delay :: Image i => i -> Delayed (ImagePixel i)
delay img = fromFunction (getSize img) (img `getPixel`)
{-# INLINE delay #-}

-- | Computes the value of an image into a manifest representation.
compute :: (Image i, Storable (ImagePixel i)) => i -> Manifest (ImagePixel i)
compute img = fromFunction (getSize img) (img `getPixel`)
{-# INLINE compute #-}
