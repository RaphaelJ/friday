{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses, PatternGuards, TypeFamilies
           , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.Type (
    -- * Classes
      Pixel (..), MaskedImage (..), Image (..), ImageChannel, FromFunction (..)
    , FunctorImage (..)
    -- * Manifest images
    , Manifest (..)
    -- * Delayed images
    , Delayed (..)
    -- * Delayed masked images
    , DelayedMask (..)
    -- * Functions
    , (!), (!?), nChannels, pixel
    -- * Conversion
    , Convertible (..), convert, delay, compute
    -- * Types helpers
    , delayed, manifest
    ) where

import Control.Applicative ((<$>))
import Data.Convertible (Convertible (..), convert)
import Data.Int
import Data.Vector.Storable (
      Vector, create, enumFromN, forM_, generate, unfoldr
    )
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (new, write)
import Data.Word
import Foreign.Storable (Storable)
import Prelude hiding (map, read)

import Vision.Primitive (
      Z (..), (:.) (..), Point, Size
    , ix2, fromLinearIndex, toLinearIndex, shapeLength
    )

-- Classes ---------------------------------------------------------------------

-- | Determines the number of channels and the type of each pixel of the image
-- and how images are represented.
class Pixel p where
    type PixelChannel p

    -- | Returns the number of channels of the pixel.
    -- Must not consume 'p' (could be 'undefined').
    pixNChannels :: p -> Int

    pixIndex :: p -> Int -> PixelChannel p

instance Pixel Int16 where
    type PixelChannel Int16 = Int16
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Int32 where
    type PixelChannel Int32 = Int32
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Int where
    type PixelChannel Int = Int
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Word8 where
    type PixelChannel Word8 = Word8
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Word16 where
    type PixelChannel Word16 = Word16
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Word32 where
    type PixelChannel Word32 = Word32
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Word where
    type PixelChannel Word = Word
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Float where
    type PixelChannel Float = Float
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Double where
    type PixelChannel Double = Double
    pixNChannels _   = 1
    pixIndex     p _ = p

instance Pixel Bool where
    type PixelChannel Bool = Bool
    pixNChannels _   = 1
    pixIndex     p _ = p

-- | Provides an abstraction for images which are not defined for each of their
-- pixels. The interface is similar to 'Image' except that indexing functions
-- don't always return.
--
-- Image origin is located in the lower left corner.
class Storable (ImagePixel i) => MaskedImage i where
    type ImagePixel i

    shape :: i -> Size

    -- | Returns the pixel\'s value at 'Z :. y, :. x'.
    maskedIndex :: i -> Point -> Maybe (ImagePixel i)
    maskedIndex img = (img `maskedLinearIndex`) . toLinearIndex (shape img)
    {-# INLINE maskedIndex #-}

    -- | Returns the pixel\'s value as if the image was a single dimension
    -- vector (row-major representation).
    maskedLinearIndex :: i -> Int -> Maybe (ImagePixel i)
    maskedLinearIndex img = (img `maskedIndex`) . fromLinearIndex (shape img)
    {-# INLINE maskedLinearIndex #-}

    -- | Returns the non-masked values of the image.
    values :: i -> Vector (ImagePixel i)
    values !img =
        unfoldr step 0
      where
        !n = shapeLength (shape img)

        step !i | i >= n                              = Nothing
                | Just p <- img `maskedLinearIndex` i = Just (p, i + 1)
                | otherwise                           = step (i + 1)
    {-# INLINE values #-}

    {-# MINIMAL shape, (maskedIndex | maskedLinearIndex) #-}

type ImageChannel i = PixelChannel (ImagePixel i)

-- | Provides an abstraction over the internal representation of an image.
-- Image origin is located in the lower left corner.
class MaskedImage i => Image i where
    -- | Returns the pixel value at 'Z :. y :. x'.
    index :: i -> Point -> ImagePixel i
    index img = (img `linearIndex`) . toLinearIndex (shape img)
    {-# INLINE index #-}

    -- | Returns the pixel value as if the image was a single dimension vector
    -- (row-major representation).
    linearIndex :: i -> Int -> ImagePixel i
    linearIndex img = (img `index`) . fromLinearIndex (shape img)
    {-# INLINE linearIndex #-}

    -- | Returns every pixel values as if the image was a single dimension
    -- vector (row-major representation).
    vector :: i -> Vector (ImagePixel i)
    vector img = generate (shapeLength $ shape img) (img `linearIndex`)
    {-# INLINE vector #-}

    {-# MINIMAL index | linearIndex #-}

-- | Provides ways to construct an image from a function.
class FromFunction i where
    type FromFunctionPixel i

    -- | Generates an image by calling the given function for each pixel of the
    -- constructed image.
    fromFunction :: Size -> (Point -> FromFunctionPixel i) -> i

    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    -- The first function is called for each line, generating a line invariant
    -- value.
    -- This function is faster for some image representations as some recurring
    -- computation can be cached.
    fromFunctionLine :: Size -> (Int -> a)
                     -> (a -> Point -> FromFunctionPixel i) -> i
    fromFunctionLine size line f =
        fromFunction size (\pt@(Z :. y :. _) -> f (line y) pt)
    {-# INLINE fromFunctionLine #-}

    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    -- The first function is called for each column, generating a column
    -- invariant value.
    -- This function *can* be faster for some image representations as some
    -- recurring computations can be cached. However, it may requires a vector
    -- allocation for these values. If the column invariant is cheap to
    -- compute, prefer 'fromFunction'.
    fromFunctionCol :: Storable b => Size -> (Int -> b)
                    -> (b -> Point -> FromFunctionPixel i) -> i
    fromFunctionCol size col f =
        fromFunction size (\pt@(Z :. _ :. x) -> f (col x) pt)
    {-# INLINE fromFunctionCol #-}

    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    -- The two first functions are called for each line and for each column,
    -- respectively, generating common line and column invariant values.
    -- This function is faster for some image representations as some recurring
    -- computation can be cached. However, it may requires a vector
    -- allocation for column values. If the column invariant is cheap to
    -- compute, prefer 'fromFunctionLine'.
    fromFunctionCached :: Storable b => Size
                       -> (Int -> a)               -- ^ Line function
                       -> (Int -> b)               -- ^ Column function
                       -> (a -> b -> Point
                           -> FromFunctionPixel i) -- ^ Pixel function
                       -> i
    fromFunctionCached size line col f =
        fromFunction size (\pt@(Z :. y :. x) -> f (line y) (col x) pt)
    {-# INLINE fromFunctionCached #-}

    {-# MINIMAL fromFunction #-}

-- | Defines a class for images on which a function can be applied. The class is
-- different from 'Functor' as there could be some constraints and
-- transformations the pixel and image types.
class (MaskedImage src, MaskedImage res) => FunctorImage src res where
    map :: (ImagePixel src -> ImagePixel res) -> src -> res

-- Manifest images -------------------------------------------------------------

-- | Stores the image content in a 'Vector'.
data Manifest p = Manifest {
      manifestSize   :: !Size
    , manifestVector :: !(Vector p)
    } deriving (Eq, Ord, Show)

instance Storable p => MaskedImage (Manifest p) where
    type ImagePixel (Manifest p) = p

    shape = manifestSize
    {-# INLINE shape #-}

    Manifest _ vec `maskedLinearIndex` ix = Just $! vec V.! ix
    {-# INLINE maskedLinearIndex #-}

    values = manifestVector
    {-# INLINE values #-}

instance Storable p => Image (Manifest p) where
    Manifest _ vec `linearIndex` ix = vec V.! ix
    {-# INLINE linearIndex #-}

    vector = manifestVector
    {-# INLINE vector #-}

instance Storable p => FromFunction (Manifest p) where
    type FromFunctionPixel (Manifest p) = p

    fromFunction !size@(Z :. h :. w) f =
        Manifest size $ create $ do
            arr <- new (h * w)

            forM_ (enumFromN 0 h) $ \y -> do
                let !lineOffset = y * w
                forM_ (enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f (ix2 y x)
                    write arr offset val

            return arr
    {-# INLINE fromFunction #-}

    fromFunctionLine !size@(Z :. h :. w) line f =
        Manifest size $ create $ do
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

    fromFunctionCol !size@(Z :. h :. w) col f =
        Manifest size $ create $ do
            -- Note: create is faster than unfoldrN.
            arr <- new (h * w)

            forM_ (enumFromN 0 h) $ \y -> do
                let !lineOffset = y * w
                forM_ (enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f (cols V.! x) (ix2 y x)
                    write arr offset val

            return arr
      where
        !cols = generate w col
    {-# INLINE fromFunctionCol #-}

    fromFunctionCached !size@(Z :. h :. w) line col f =
        Manifest size $ create $ do
            -- Note: create is faster than unfoldrN.
            arr <- new (h * w)

            forM_ (enumFromN 0 h) $ \y -> do
                let !lineVal    = line y
                    !lineOffset = y * w
                forM_ (enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f lineVal (cols V.! x) (ix2 y x)
                    write arr offset val

            return arr
      where
        !cols = generate w col
    {-# INLINE fromFunctionCached #-}

instance (Image src, Storable p) => FunctorImage src (Manifest p) where
    map f img = fromFunction (shape img) (f . (img !))
    {-# INLINE map #-}

-- Delayed images --------------------------------------------------------------

-- | A delayed image is an image which is constructed using a function.
--
-- Usually, a delayed image maps each of its pixels over another image.
-- Delayed images are useful by avoiding intermediate images in a
-- transformation pipeline of images or by avoiding the computation of the whole
-- resulting image when only a portion of its pixels will be accessed.
data Delayed p = Delayed {
      delayedSize :: !Size
    , delayedFun  :: !(Point -> p)
    }

instance Storable p => MaskedImage (Delayed p) where
    type ImagePixel (Delayed p) = p

    shape = delayedSize
    {-# INLINE shape #-}

    maskedIndex img = Just . delayedFun img
    {-# INLINE maskedIndex #-}

instance Storable p => Image (Delayed p) where
    index = delayedFun
    {-# INLINE index #-}

instance FromFunction (Delayed p) where
    type FromFunctionPixel (Delayed p) = p

    fromFunction = Delayed
    {-# INLINE fromFunction #-}

instance (Image src, Storable p) => FunctorImage src (Delayed p) where
    map f img = fromFunction (shape img) (f . (img !))
    {-# INLINE map #-}

-- Masked delayed images -------------------------------------------------------

data DelayedMask p = DelayedMask {
      delayedMaskSize :: !Size
    , delayedMaskFun  :: !(Point -> Maybe p)
    }

instance Storable p => MaskedImage (DelayedMask p) where
    type ImagePixel (DelayedMask p) = p

    shape = delayedMaskSize
    {-# INLINE shape #-}

    maskedIndex = delayedMaskFun
    {-# INLINE maskedIndex #-}

instance Storable p => FromFunction (DelayedMask p) where
    type FromFunctionPixel (DelayedMask p) = Maybe p

    fromFunction = DelayedMask
    {-# INLINE fromFunction #-}

instance (MaskedImage src, Storable p) => FunctorImage src (DelayedMask p) where
    map f img = fromFunction (shape img) (\pt -> f <$> (img `maskedIndex` pt))
    {-# INLINE map #-}

-- Functions -------------------------------------------------------------------

-- | Alias of 'maskedIndex'.
(!?) :: MaskedImage i => i -> Point -> Maybe (ImagePixel i)
(!?) = maskedIndex
{-# INLINE (!?) #-}

-- | Alias of 'index'.
(!) :: Image i => i -> Point -> ImagePixel i
(!) = index
{-# INLINE (!) #-}

-- | Returns the number of channels of an image.
nChannels :: (Pixel (ImagePixel i), MaskedImage i) => i -> Int
nChannels img = pixNChannels (pixel img)
{-# INLINE nChannels #-}

-- | Returns an 'undefined' instance of a pixel of the image. This is sometime
-- useful to satisfy the type checker as in a call to 'pixNChannels' :
--
-- > nChannels img = pixNChannels (pixel img)
pixel :: MaskedImage i => i -> ImagePixel i
pixel _ = undefined

-- Conversion ------------------------------------------------------------------

-- | Delays an image in its delayed representation.
delay :: Image i => i -> Delayed (ImagePixel i)
delay = map id
{-# INLINE delay #-}

-- | Computes the value of an image into a manifest representation.
compute :: (Image i, Storable (ImagePixel i)) => i -> Manifest (ImagePixel i)
compute = map id
{-# INLINE compute #-}

instance (Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Manifest p1) (Manifest p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Delayed p1) (Delayed p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Delayed p1) (Manifest p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Manifest p1) (Delayed  p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

-- Types helpers ---------------------------------------------------------------------

-- | Forces an image to be in its delayed represenation. Does nothing.
delayed :: Delayed p -> Delayed p
delayed = id

-- | Forces an image to be in its manifest represenation. Does nothing.
manifest :: Manifest p -> Manifest p
manifest = id
