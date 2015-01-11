{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}

-- | Provides high level filtering functions for images.
--
-- Use 'Vision.Image.Filter.Internal' if you want to create new image filters.
--
-- Filters are operations on images on which the surrounding of each processed
-- pixel is considered according to a kernel.
--
-- See <http://en.wikipedia.org/wiki/Kernel_(image_processing)> for details.
--
-- The @radius@ argument of some filters is used to determine the kernel size.
-- A radius as of 1 means a kernel of size 3, 2 a kernel of size 5 and so on.
--
-- /Note:/ filters are currently not supported on multi-channel images (RGB,
-- RGBA ...) are currently not supported.
module Vision.Image.Filter (
    -- * Classes and type
      Filterable, Filter, SeparatelyFiltrable
    -- * Morphological operators
    , dilate, erode
    -- * Blur
    , blur, gaussianBlur
    -- * Derivation
    , DerivativeType (..), scharr, sobel
    -- * Others
    , mean
    ) where

import Data.Int
import Foreign.Storable (Storable)

import Vision.Image.Class (MaskedImage (..), Image (..), FromFunction (..))
import Vision.Image.Filter.Internal (
      Filterable, Filter, SeparatelyFiltrable (..)
    , DerivativeType
    )
import Vision.Primitive (Size)

import qualified Vision.Image.Filter.Internal as Internal

-- Morphological operators -----------------------------------------------------

dilate, erode :: ( Image src, Ord (ImagePixel src)
                 , FromFunction res, FromFunctionPixel res ~ ImagePixel src
                 , SeparatelyFiltrable src res (ImagePixel src))
       => Int           -- ^ Kernel radius.
       -> src
       -> res

dilate radius img = Internal.dilate radius `Internal.apply` img
{-# INLINABLE dilate #-}

erode  radius img = Internal.erode radius `Internal.apply` img
{-# INLINABLE erode #-}

-- Blur ------------------------------------------------------------------------

-- | Blurs the image by averaging the pixel inside the kernel.
--
-- Uses an 'Int32' as accumulator during the averaging operation.
blur :: ( Image src, Integral (ImagePixel src)
        , FromFunction res, Num (FromFunctionPixel res)
        , SeparatelyFiltrable src res Int32)
       => Int           -- ^ Blur radius.
       -> src
       -> res
blur radius img =
    let filt :: (Integral src, Num res) => Internal.Blur src Int32 res
        filt = Internal.blur radius
    in filt `Internal.apply` img
{-# INLINABLE blur #-}

-- | Blurs the image by averaging the pixel inside the kernel using a Gaussian
-- function.
--
-- See <http://en.wikipedia.org/wiki/Gaussian_blur>
gaussianBlur :: ( Image src, Integral (ImagePixel src)
                , FromFunction res, Integral (FromFunctionPixel res)
                , Floating acc, RealFrac acc, Storable acc
                , SeparatelyFiltrable src res acc)
             => Int     -- ^ Blur radius.
             -> Maybe acc
             -- ^ Sigma value of the Gaussian function. If not given, will be
             -- automatically computed from the radius so that the kernel
             -- fits 3σ of the distribution.
             -> src
             -> res
gaussianBlur radius mSig img =
    Internal.gaussianBlur radius mSig `Internal.apply` img
{-# INLINABLE gaussianBlur #-}

-- Derivation ------------------------------------------------------------------

-- | Estimates the first derivative using the Scharr's 3x3 kernel.
--
-- Convolves the following kernel for the X derivative:
--
-- @
--  -3   0   3
-- -10   0  10
--  -3   0   3
-- @
--
-- And this kernel for the Y derivative:
--
-- @
--  -3 -10  -3
--   0   0   0
--   3  10   3
-- @
--
-- Uses an 'Int32' as accumulator during kernel application.
scharr :: ( Image src, Integral (ImagePixel src)
          , FromFunction res, Integral (FromFunctionPixel res)
          , Storable (FromFunctionPixel res)
          , SeparatelyFiltrable src res (FromFunctionPixel res))
       => DerivativeType -> src -> res
scharr der img = Internal.scharr der `Internal.apply` img
{-# INLINABLE scharr #-}

-- | Estimates the first derivative using a Sobel's kernel.
--
-- Prefer 'scharr' when radius equals @1@ as Scharr's kernel is more accurate
-- and is implemented faster.
--
-- Uses an 'Int32' as accumulator during kernel application.
sobel :: ( Image src, Integral (ImagePixel src)
          , FromFunction res, Integral (FromFunctionPixel res)
          , Storable (FromFunctionPixel res)
          , SeparatelyFiltrable src res (FromFunctionPixel res))
      => Int            -- ^ Kernel radius.
      -> DerivativeType
      -> src
      -> res
sobel radius der img = Internal.sobel radius der `Internal.apply` img
{-# INLINABLE sobel #-}

-- Others ----------------------------------------------------------------------

-- | Computes the average of a kernel of the given size.
--
-- This is similar to 'blur' but with a rectangular kernel and a 'Fractional'
-- result.
--
-- Uses an 'Int32' as accumulator during the averaging operation.
mean :: ( Image src, Integral (ImagePixel src)
        , FromFunction res, Fractional (FromFunctionPixel res)
        , SeparatelyFiltrable src res Int32)
     => Size -> src -> res
mean size img =
    let filt :: (Integral src, Fractional res) => Internal.Mean src Int32 res
        filt = Internal.mean size
    in filt `Internal.apply` img
{-# INLINABLE mean #-}
