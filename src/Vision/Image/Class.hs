{-# LANGUAGE BangPatterns, FlexibleContexts, MultiParamTypeClasses
           , TypeFamilies #-}

module Vision.Image.Class (
    -- * Classes
    -- ** Image access
      Pixel (..)
    , MaskedImage (..), MaskedImageValues (..)
    , Image (..), ImageVector (..), ImageChannel
    -- ** Image construction
    , FromFunctionPixel, FromFunction (..), FromFunctionLine (..)
    , FromFunctionCol (..), FromFunctionLineCol (..)
    -- ** Image transformations
    , FunctorImage (..)
    -- * Functions
    , (!), (!?), nChannels, pixel
    ) where

import Data.Int
import Data.Maybe (fromJust)
import Data.Vector.Storable (Vector, generate, unfoldr)
import Data.Word
import Foreign.Storable (Storable)
import Prelude hiding (map, read)

import Vision.Primitive (
      Point, Size, fromLinearIndex, toLinearIndex, shapeLength
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
-- pixels.
--
-- The interface is similar to 'Image' except that indexing functions don't
-- always return.
--
-- Image origin (@'ix2' 0 0@) is located in the upper left corner.
class MaskedImage i where
    type ImagePixel i

    shape :: i -> Size

    -- | Returns the pixel\'s value at 'Z :. y, :. x'.
    maskedIndex :: i -> Point -> Maybe (ImagePixel i)
    maskedIndex !img = maskedLinearIndex img . toLinearIndex (shape img)
    {-# INLINE maskedIndex #-}

    -- | Returns the pixel\'s value as if the image was a single dimension
    -- vector (row-major representation).
    maskedLinearIndex :: i -> Int -> Maybe (ImagePixel i)
    maskedLinearIndex !img = maskedIndex img . fromLinearIndex (shape img)
    {-# INLINE maskedLinearIndex #-}

    type LineConstant   i
    type ColumnConstant i

    lineConstant   :: i -> Int -> LineConstant   i
    columnConstant :: i -> Int -> ColumnConstant i

    -- | For some representations, some computed values are constant for a given
    -- line and/or column. 'maskedCachedIndex' tries to avoid to recompute these
    -- values.
    --
    -- You should prefer 'maskedCachedIndex' to 'maskedIndex' when iterating
    -- over the whole image sequentially, as it's more efficient.
    --
    -- Exemple:
    --
    -- > let Z :. height :. width = shape srcImg
    -- >
    -- > forM_ [0..height-1] $ \y -> do
    -- >     let lineConst = lineConstant y
    -- >     forM_ [0..width-1] $ \x -> do
    -- >         let colConst = columnConstant x
    -- >             pixelVal = maskedCachedIndex srcImg lineConst colConst
    -- >                                          (ix2 y x)
    -- >             print pixelVal
    maskedCachedIndex :: i -> LineConstant i -> ColumnConstant i -> Point
                      -> Maybe (ImagePixel i)
    maskedCachedIndex !img _ _ !pt = maskedIndex img pt
    {-# INLINE maskedCachedIndex #-}

    -- | For some representations, some computed values are constant for a given
    -- line and/or column. 'maskedCachedLinearIndex' tries to avoid to recompute
    -- these values.
    --
    -- You should prefer 'maskedCachedLinearIndex' to 'maskedLinearIndex' when
    -- iterating over the whole image sequentially, as it's more efficient.
    --
    -- See 'maskedCachedIndex' for an example.
    maskedCachedLinearIndex :: i -> LineConstant i -> ColumnConstant i -> Int
                            -> Maybe (ImagePixel i)
    maskedCachedLinearIndex !img _ _ !ix = maskedLinearIndex img ix
    {-# INLINE maskedCachedLinearIndex #-}

    {-# MINIMAL shape, (maskedIndex | maskedLinearIndex), lineConstant
              , columnConstant #-}

class (MaskedImage i, Storable (ImagePixel i)) => MaskedImageValues i where
    -- | Returns the non-masked values of the image.
    --
    -- Similar to 'vector' but for masked images.
    values :: i -> Vector (ImagePixel i)
    values !img =
        unfoldr step 0
      where
        !n = shapeLength (shape img)

        step !i | i >= n                              = Nothing
                | Just p <- img `maskedLinearIndex` i = Just (p, i + 1)
                | otherwise                           = step (i + 1)
    {-# INLINE values #-}

    {-# MINIMAL #-}

type ImageChannel i = PixelChannel (ImagePixel i)

-- | Provides an abstraction over the internal representation of an image.
--
-- Unlike 'MaskedImage's, 'Image's are entierly defined, i.e. they have no pixel
-- for which 'maskedIndex' returns 'Nothing'.
--
-- Image origin (@'ix2' 0 0@) is located in the upper left corner.
class MaskedImage i => Image i where
    -- | Returns the pixel value at 'Z :. y :. x'.
    index :: i -> Point -> ImagePixel i
    index !img = fromJust . maskedIndex img
    {-# INLINE index #-}

    -- | Returns the pixel value as if the image was a single dimension vector
    -- (row-major representation).
    linearIndex :: i -> Int -> ImagePixel i
    linearIndex !img = fromJust . maskedLinearIndex img
    {-# INLINE linearIndex #-}

    -- | For some representations, some computed values are constant for a given
    -- line and/or column. 'cachedIndex' tries to avoid to recompute these
    -- values.
    --
    -- You should prefer 'cachedIndex' to 'index' when iterating over the whole
    -- image sequentially, as it's more efficient.
    --
    -- See 'maskedCachedIndex' for an example.
    cachedIndex :: i -> LineConstant i -> ColumnConstant i -> Point
                -> ImagePixel i
    cachedIndex !img !line !col = fromJust . maskedCachedIndex img line col
    {-# INLINE cachedIndex #-}

    -- | For some representations, some computed values are constant for a given
    -- line and/or column. 'cachedLinearIndex' tries to avoid to recompute these
    -- values.
    --
    -- You should prefer 'cachedLinearIndex' to 'linearIndex' when iterating
    -- over the whole image sequentially, as it's more efficient.
    --
    -- See 'maskedCachedIndex' for an example.
    cachedLinearIndex :: i -> LineConstant i -> ColumnConstant i -> Int
                      -> ImagePixel i
    cachedLinearIndex !img _ _ !ix = linearIndex img ix
    {-# INLINE cachedLinearIndex #-}

    {-# MINIMAL #-}

class (Image i, Storable (ImagePixel i)) => ImageVector i where
    -- | Returns every pixel values as if the image was a single dimension
    -- vector (row-major representation).
    --
    -- Similar to 'values' but for unmasked images.
    vector :: i -> Vector (ImagePixel i)
    vector img = generate (shapeLength $ shape img) (img `linearIndex`)
    {-# INLINE vector #-}

    {-# MINIMAL #-}

-- | Pixel type which must be returned by functions which create an image from
-- a function.
--
-- 'FromFunctionPixel' could be different from 'ImagePixel' for some
-- representations. For exemple, with maskable images, if 'ImagePixel' is 'p'
-- then 'FromFunctionPixel' is 'Maybe p'.
type family FromFunctionPixel i

-- | Provides ways to construct an image from a function.
class FromFunction i where

    -- | Generates an image by calling the given function for each pixel of the
    -- constructed image.
    fromFunction :: Size -> (Point -> FromFunctionPixel i) -> i

    {-# MINIMAL fromFunction #-}

class FromFunctionLine i l where

    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    --
    -- The first function is called for each line, generating a line constant
    -- value.
    --
    -- This function is faster for some image representations than
    -- 'fromFunction' as some recurring computations can be cached.
    fromFunctionLine :: Size -> (Int -> l)
                     -> (l -> Point -> FromFunctionPixel i) -> i

    {-# MINIMAL fromFunctionLine #-}

class FromFunctionCol i c where
    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    --
    -- The first function is called for each column, generating a column
    -- constant value.
    --
    -- This function *can* be faster for some image representations as some
    -- recurring computations can be cached. However, it may requires a vector
    -- allocation for these values. If the column constant is cheap to
    -- compute, prefer 'fromFunction'.
    fromFunctionCol :: Size -> (Int -> c)
                    -> (c -> Point -> FromFunctionPixel i) -> i

    {-# MINIMAL fromFunctionCol #-}

class FromFunctionLineCol i l c where
    -- | Generates an image by calling the last function for each pixel of the
    -- constructed image.
    --
    -- The two first functions are called for each line and for each column,
    -- respectively, generating common line and column constant values.
    --
    -- This function is faster for some image representations as some recurring
    -- computations can be cached. However, it may requires a vector
    -- allocation for column values. If the column constant is cheap to
    -- compute, prefer 'fromFunctionLine'.
    fromFunctionLineCol :: Size
                        -> (Int -> l)               -- ^ Line function
                        -> (Int -> c)               -- ^ Column function
                        -> (l -> c -> Point
                            -> FromFunctionPixel i) -- ^ Pixel function
                        -> i

    {-# MINIMAL fromFunctionLineCol #-}

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
