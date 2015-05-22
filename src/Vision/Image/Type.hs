{-# LANGUAGE BangPatterns
           , CPP
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , PatternGuards
           , TypeFamilies
           , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.Type (
    -- * Manifest images
      Manifest (..)
    -- * Delayed images
    , Delayed (..)
    -- * Delayed masked images
    , DelayedMask (..)
    -- * Conversion and type helpers
    , delay, compute, delayed, manifest
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Control.DeepSeq (NFData (..))
import Data.Convertible (Convertible (..), convert)
import Foreign.Storable (Storable)
import Prelude hiding (map, read)

import qualified Data.Vector                    as V
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable   as VSM

import Vision.Image.Class (
      MaskedImage (..), MaskedImageValues (..)
    , Image (..), ImageVector (..)
    , FromFunction (..), FromFunctionLine (..), FromFunctionCol (..)
    , FromFunctionLineCol (..)
    , FunctorImage (..)
    )
import Vision.Primitive (Z (..), (:.) (..), Point, Size, ix2)

-- Manifest images -------------------------------------------------------------

-- | Stores the image content in a 'VS.Vector'.
data Manifest p = Manifest {
      manifestSize   :: !Size
    , manifestVector :: !(VS.Vector p)
    } deriving (Eq, Ord, Show)

instance NFData (Manifest p) where
    rnf !_ = ()

instance Storable p => MaskedImage (Manifest p) where
    type ImagePixel (Manifest p) = p

    shape = manifestSize
    {-# INLINE shape #-}

    Manifest _ vec `maskedLinearIndex` ix = Just $! vec VS.! ix
    {-# INLINE maskedLinearIndex #-}

    -- Manifest images don't have any constant values to be cached when
    -- iterated.

    type LineConstant   (Manifest p) = ()
    type ColumnConstant (Manifest p) = ()

    lineConstant   _ = const ()
    {-# INLINE lineConstant #-}

    columnConstant _ = const ()
    {-# INLINE columnConstant #-}

instance Storable p => MaskedImageValues (Manifest p) where
    values = manifestVector
    {-# INLINE values #-}

instance Storable p => Image (Manifest p) where
    Manifest _ vec `linearIndex` ix = vec VS.! ix
    {-# INLINE linearIndex #-}

instance Storable p => ImageVector (Manifest p) where 
    vector = manifestVector
    {-# INLINE vector #-}

instance Storable p => FromFunction (Manifest p) where
    type FromFunctionPixel (Manifest p) = p

    fromFunction !size@(Z :. h :. w) f =
        Manifest size $ VS.create $ do
            -- Note: create is faster than unfoldrN.
            arr <- VSM.new (h * w)

            VS.forM_ (VS.enumFromN 0 h) $ \y -> do
                let !lineOffset = y * w
                VS.forM_ (VS.enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f (ix2 y x)
                    VSM.write arr offset val

            return arr
    {-# INLINE fromFunction #-}

instance Storable p => FromFunctionLine (Manifest p) l where
    fromFunctionLine !size@(Z :. h :. w) line f =
        Manifest size $ VS.create $ do
            -- Note: create is faster than unfoldrN.
            arr <- VSM.new (h * w)

            VS.forM_ (VS.enumFromN 0 h) $ \y -> do
                let !lineVal    = line y
                    !lineOffset = y * w
                VS.forM_ (VS.enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f lineVal (ix2 y x)
                    VSM.write arr offset val

            return arr
    {-# INLINE fromFunctionLine #-}

instance Storable p => FromFunctionCol (Manifest p) c where
    fromFunctionCol !size@(Z :. h :. w) col f =
        Manifest size $ VS.create $ do
            -- Note: create is faster than unfoldrN.
            arr <- VSM.new (h * w)

            VS.forM_ (VS.enumFromN 0 h) $ \y -> do
                let !lineOffset = y * w
                VS.forM_ (VS.enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f (cols V.! x) (ix2 y x)
                    VSM.write arr offset val

            return arr
      where
        !cols = V.generate w col
    {-# INLINE fromFunctionCol #-}

instance Storable p => FromFunctionLineCol (Manifest p) l c where
    fromFunctionLineCol !size@(Z :. h :. w) line col f =
        Manifest size $ VS.create $ do
            -- Note: create is faster than unfoldrN.
            arr <- VSM.new (h * w)

            VS.forM_ (VS.enumFromN 0 h) $ \y -> do
                let !lineVal    = line y
                    !lineOffset = y * w
                VS.forM_ (VS.enumFromN 0 w) $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f lineVal (cols V.! x) (ix2 y x)
                    VSM.write arr offset val

            return arr
      where
        !cols = V.generate w col
    {-# INLINE fromFunctionLineCol #-}

instance (Image src, Storable p) => FunctorImage src (Manifest p) where
    map f img = fromFunctionLineCol (shape img) (lineConstant img)
                                    (columnConstant img)
                                    (\l c pt -> f $! cachedIndex img l c pt)
    {-# INLINE map #-}

-- Delayed images --------------------------------------------------------------

-- | A delayed image is an image which is constructed using a function.
--
-- Usually, a delayed image maps each of its pixels over another image.
--
-- Delayed images are useful by avoiding intermediate images in a
-- transformation pipeline of images or by avoiding the computation of the whole
-- resulting image when only a portion of its pixels will be accessed.
data Delayed p = Delayed {
      delayedSize :: !Size
    , delayedFun  :: !(Point -> p)
    }

instance MaskedImage (Delayed p) where
    type ImagePixel (Delayed p) = p

    shape = delayedSize
    {-# INLINE shape #-}

    maskedIndex img = Just . delayedFun img
    {-# INLINE maskedIndex #-}

    type LineConstant   (Delayed p) = ()
    type ColumnConstant (Delayed p) = ()

    lineConstant   _ = const ()
    {-# INLINE lineConstant #-}

    columnConstant _ = const ()
    {-# INLINE columnConstant #-}

instance Storable p => MaskedImageValues (Delayed p) where

instance Image (Delayed p) where
    index = delayedFun
    {-# INLINE index #-}

instance Storable p => ImageVector (Delayed p) where

instance FromFunction (Delayed p) where
    type FromFunctionPixel (Delayed p) = p

    fromFunction = Delayed
    {-# INLINE fromFunction #-}

instance FromFunctionLine (Delayed p) l where

instance FromFunctionCol (Delayed p) c where

instance FromFunctionLineCol (Delayed p) l c where

instance Image src => FunctorImage src (Delayed p) where
    map f img = fromFunctionLineCol (shape img) (lineConstant img)
                                    (columnConstant img)
                                    (\l c pt -> f $! cachedIndex img l c pt)
    {-# INLINE map #-}

-- Masked delayed images -------------------------------------------------------

data DelayedMask p = DelayedMask {
      delayedMaskSize :: !Size
    , delayedMaskFun  :: !(Point -> Maybe p)
    }

instance MaskedImage (DelayedMask p) where
    type ImagePixel (DelayedMask p) = p

    shape = delayedMaskSize
    {-# INLINE shape #-}

    maskedIndex = delayedMaskFun
    {-# INLINE maskedIndex #-}

    type LineConstant   (DelayedMask p) = ()
    type ColumnConstant (DelayedMask p) = ()

    lineConstant   _ = const ()
    {-# INLINE lineConstant #-}

    columnConstant _ = const ()
    {-# INLINE columnConstant #-}

instance Storable p => MaskedImageValues (DelayedMask p) where

instance FromFunction (DelayedMask p) where
    type FromFunctionPixel (DelayedMask p) = Maybe p

    fromFunction = DelayedMask
    {-# INLINE fromFunction #-}

instance FromFunctionLine (DelayedMask p) l where

instance FromFunctionCol (DelayedMask p) c where

instance FromFunctionLineCol (DelayedMask p) l c where

instance MaskedImage src => FunctorImage src (DelayedMask p) where
    map f img = fromFunction (shape img) (\pt -> f <$> (img `maskedIndex` pt))
    {-# INLINE map #-}


-- Conversion and type helpers -------------------------------------------------

-- | Delays an image in its delayed representation.
delay :: Image i => i -> Delayed (ImagePixel i)
delay = map id
{-# INLINE delay #-}

-- | Computes the value of an image into a manifest representation.
compute :: (Image i, Storable (ImagePixel i), Storable (ColumnConstant i))
        => i -> Manifest (ImagePixel i)
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

-- | Forces an image to be in its delayed represenation. Does nothing.
delayed :: Delayed p -> Delayed p
delayed = id

-- | Forces an image to be in its manifest represenation. Does nothing.
manifest :: Manifest p -> Manifest p
manifest = id
