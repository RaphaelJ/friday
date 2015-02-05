{-# LANGUAGE BangPatterns, FlexibleContexts, MultiParamTypeClasses
           , TypeFamilies #-}

module Vision.Image.Class (
    -- * Classes
      Pixel (..), MaskedImage (..), Image (..), ImageChannel, FromFunction (..)
    , FunctorImage (..)
    -- * Functions
    , (!), (!?), nChannels, pixel
    -- * Conversion
    , Convertible (..), convert
    ) where

import Data.Convertible (Convertible (..), convert)
import Data.Int
import Data.Vector.Storable (Vector, generate, unfoldr)
import Data.Word
import Foreign.Storable (Storable)
import Prelude hiding (map, read)

import Vision.Primitive (
      Z (..), (:.) (..), Point, Size
    , fromLinearIndex, toLinearIndex, shapeLength
    )

-- Classes ---------------------------------------------------------------------

-- | Determines the number of channels and the type of each pixel of the image
-- and how images are represented.
class Pixel p where
    type PixelChannel p

    -- | Returns the number of channels of the pixel.
    --
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
-- Image origin (@'ix2' 0 0@) is located in the upper left corner.
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
--
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
    --
    -- The first function is called for each line, generating a line invariant
    -- value.
    --
    -- This function is faster for some image representations as some recurring
    -- computation can be cached.
    fromFunctionLine :: Size -> (Int -> a)
                     -> (a -> Point -> FromFunctionPixel i) -> i
    fromFunctionLine size line f =
        fromFunction size (\pt@(Z :. y :. _) -> f (line y) pt)
    {-# INLINE fromFunctionLine #-}

    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    --
    -- The first function is called for each column, generating a column
    -- invariant value.
    --
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
    --
    -- The two first functions are called for each line and for each column,
    -- respectively, generating common line and column invariant values.
    --
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
