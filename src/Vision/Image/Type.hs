{-# LANGUAGE BangPatterns
           , CPP
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , PatternGuards
           , RecordWildCards
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
    , FromFunctionPixel, FromFunction (..), FromFunctionLine (..)
    , FromFunctionCol (..), FromFunctionLineCol (..)
    , FunctorImage (..)
    )
import Vision.Primitive (
      Z (..), (:.) (..), Point, Size, ix2, fromLinearIndex, toLinearIndex
    )

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

    maskedCachedIndex img _ _ pt = maskedIndex img pt
    {-# INLINE maskedCachedIndex #-}

    maskedCachedLinearIndex img _ _ ix = maskedLinearIndex img ix
    {-# INLINE maskedCachedLinearIndex #-}

instance Storable p => MaskedImageValues (Manifest p) where
    values = manifestVector
    {-# INLINE values #-}

instance Storable p => Image (Manifest p) where
    index !img = linearIndex img . toLinearIndex (shape img)
    {-# INLINE index #-}

    Manifest _ vec `linearIndex` ix = vec VS.! ix
    {-# INLINE linearIndex #-}

    cachedIndex !img _ _ = index img
    {-# INLINE cachedIndex #-}

    cachedLinearIndex !img _ _ = linearIndex img
    {-# INLINE cachedLinearIndex #-}

instance Storable p => ImageVector (Manifest p) where 
    vector = manifestVector
    {-# INLINE vector #-}

type instance FromFunctionPixel (Manifest p) = p

instance Storable p => FromFunction (Manifest p) where

    fromFunction !size@(Z :. h :. w) f =
        Manifest size $ VS.create $ do
            -- Note: create is faster than unfoldrN.
            arr <- VSM.new (h * w)

            forRangeM_ 0 h $ \y -> do
                let !lineOffset = y * w
                forRangeM_ 0 w $ \x -> do
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

            forRangeM_ 0 h $ \y -> do
                let !lineVal    = line y
                    !lineOffset = y * w
                forRangeM_ 0 w $ \x -> do
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

            forRangeM_ 0 h $ \y -> do
                let !lineOffset = y * w
                forRangeM_ 0 w $ \x -> do
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

            forRangeM_ 0 h $ \y -> do
                let !lineVal    = line y
                    !lineOffset = y * w
                forRangeM_ 0 w $ \x -> do
                    let !offset = lineOffset + x
                        !val    = f lineVal (cols V.! x) (ix2 y x)
                    VSM.write arr offset val

            return arr
      where
        !cols = V.generate w col
    {-# INLINE fromFunctionLineCol #-}

-- These rules replaces the call to 'fromFunctionLine', 'fromFunctionCol' or
-- 'fromFunctionLineCol' to call to 'fromFunction', 'fromFunctionLine' or
-- 'fromFunctionCol' when LineConstant = () or ColumnConstant = ().
{-
{-# RULES
"fromFunctionLine with LineConstant = ()"
    forall size line (f :: Storable p => () -> Point -> p).
    fromFunctionLine size line f = fromFunction size (f ())

"fromFunctionCol with ColumnConstant = ()"
    forall size col (f :: Storable p => () -> Point -> p).
    fromFunctionCol size col f = fromFunction size (f ())

"fromFunctionLineCol with LineConstant = () and ColumnConstant = ()"
    forall size line col (f :: Storable p => () -> () -> Point -> p).
    fromFunctionLineCol size line col f = fromFunction size (f () ())

"fromFunctionLineCol with LineConstant = ()"
    forall size line col (f :: Storable p => () -> c -> Point -> p).
    fromFunctionLineCol size line col f = fromFunctionCol size col (f ())

"fromFunctionLineCol with ColumnConstant = ()"
    forall size line col (f :: Storable p => l -> () -> Point -> p).
    fromFunctionLineCol size line col f = fromFunctionLine size line
                                                           (\l -> f l ())
 #-}-}

-- These rules replace call to 'fromFunctionCol' and 'fromFunctionLineCol' with
-- Storable vector instances when ColumnConstant is an instance of Storable.

{-# RULES
"fromFunctionCol with ColumnConstant being an Storable instance"
    forall size  col (f :: (Storable c, Storable p) => c -> Point -> p).
    fromFunctionCol size col f = fromFunctionColStorable size col f

"fromFunctionLineCol with ColumnConstant being an Storable instance"
    forall size line col (f :: (Storable c, Storable p)
                            => l -> c -> Point -> p).
    fromFunctionLineCol size line col f = fromFunctionLineColStorable size line
                                                                      col f
 #-}

-- | 'fromFunctionCol' specialized for 'Storable' 'ColumnConstant'.
fromFunctionColStorable :: (Storable p, Storable c)
                        => Size -> (Int -> c) -> (c ->  Point -> p)
                        -> Manifest p
fromFunctionColStorable !size@(Z :. h :. w) col f =
    Manifest size $ VS.create $ do
        -- Note: create is faster than unfoldrN.
        arr <- VSM.new (h * w)

        forRangeM_ 0 h $ \y -> do
            let !lineOffset = y * w
            forRangeM_ 0 w $ \x -> do
                let !offset = lineOffset + x
                    !val    = f (cols VS.! x) (ix2 y x)
                VSM.write arr offset val

        return arr
  where
    !cols = VS.generate w col
{-# INLINE fromFunctionColStorable #-}

-- | 'fromFunctionLineCol' specialized for 'Storable' 'ColumnConstant'.
fromFunctionLineColStorable :: (Storable p, Storable c)
                            => Size -> (Int -> l) -> (Int -> c)
                            -> (l -> c ->  Point -> p)
                            -> Manifest p
fromFunctionLineColStorable !size@(Z :. h :. w) line col f =
    Manifest size $ VS.create $ do
        -- Note: create is faster than unfoldrN.
        arr <- VSM.new (h * w)

        forRangeM_ 0 h $ \y -> do
            let !lineVal    = line y
                !lineOffset = y * w
            forRangeM_ 0 w $ \x -> do
                let !offset = lineOffset + x
                    !val    = f lineVal (cols VS.! x) (ix2 y x)
                VSM.write arr offset val

        return arr
  where
    !cols = VS.generate w col
{-# INLINE fromFunctionLineColStorable #-}

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
data Delayed l c p = Delayed {
      delayedSize :: !Size
    , delayedLine :: !(Int -> l)
    , delayedCol  :: !(Int -> c)
    , delayedFun  :: !(l -> c -> Point -> p)
    }

instance MaskedImage (Delayed l c p) where
    type ImagePixel (Delayed l c p) = p

    shape = delayedSize
    {-# INLINE shape #-}

    maskedIndex !img@Delayed {..} !pt@(Z :. y :. x) =
        maskedCachedIndex img (delayedLine y) (delayedCol x) pt
    {-# INLINE maskedIndex #-}

    type LineConstant   (Delayed l c p) = l
    type ColumnConstant (Delayed l c p) = c

    lineConstant = delayedLine
    {-# INLINE lineConstant #-}

    columnConstant = delayedCol
    {-# INLINE columnConstant #-}

    maskedCachedIndex !Delayed {..} !line !col !pt =
        Just $! delayedFun line col pt
    {-# INLINE maskedCachedIndex #-}

    maskedCachedLinearIndex !img !line !col =
        maskedCachedIndex img line col . fromLinearIndex (shape img)
    {-# INLINE maskedCachedLinearIndex #-}

instance Storable p => MaskedImageValues (Delayed l c p) where

instance Image (Delayed l c p) where
    index !img@Delayed {..} !pt@(Z :. y :. x) =
        cachedIndex img (delayedLine y) (delayedCol x) pt
    {-# INLINE index #-}

    linearIndex !img = index img . fromLinearIndex (shape img)
    {-# INLINE linearIndex #-}

    cachedIndex = delayedFun
    {-# INLINE cachedIndex #-}

    cachedLinearIndex !img !line !col =
        cachedIndex img line col . fromLinearIndex (shape img)
    {-# INLINE cachedLinearIndex #-}

instance Storable p => ImageVector (Delayed l c p) where

type instance FromFunctionPixel (Delayed l c p) = p

instance FromFunction (Delayed () () p) where

    fromFunction size f = Delayed size (const ()) (const ()) (\_ _ -> f)
    {-# INLINE fromFunction #-}

instance FromFunctionLine (Delayed l () p) l where
    fromFunctionLine size line f = Delayed size line (const ()) (\l _ -> f l)
    {-# INLINE fromFunctionLine #-}

instance FromFunctionCol (Delayed () c p) c where
    fromFunctionCol size col f = Delayed size (const ()) col (\_ c -> f c)
    {-# INLINE fromFunctionCol #-}

instance FromFunctionLineCol (Delayed l c p) l c where
    fromFunctionLineCol = Delayed
    {-# INLINE fromFunctionLineCol #-}

instance (Image src, LineConstant src ~ l, ColumnConstant src ~ c)
        => FunctorImage src (Delayed l c p) where
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

type instance FromFunctionPixel (DelayedMask p) = Maybe p

instance FromFunction (DelayedMask p) where
    fromFunction = DelayedMask
    {-# INLINE fromFunction #-}

instance FromFunctionLine (DelayedMask p) l where
    fromFunctionLine !size !line !f =
        fromFunction size (\pt@(Z :. y :. _) -> f (line y) pt)
    {-# INLINE fromFunctionLine #-}

instance FromFunctionCol (DelayedMask p) c where
    fromFunctionCol !size !col !f =
         fromFunction size (\pt@(Z :. _ :. x) -> f (col x) pt)
    {-# INLINE fromFunctionCol #-}

instance FromFunctionLineCol (DelayedMask p) l c where
    fromFunctionLineCol !size !line !col !f =
         fromFunction size (\pt@(Z :. y :. x) -> f (line y) (col x) pt)
    {-# INLINE fromFunctionLineCol #-}

instance MaskedImage src => FunctorImage src (DelayedMask p) where
    map f img = fromFunction (shape img) (\pt -> f <$> (img `maskedIndex` pt))
    {-# INLINE map #-}

-- Conversion and type helpers -------------------------------------------------

-- | Delays an image in its delayed representation.
delay :: Image i
      => i -> Delayed (LineConstant i) (ColumnConstant i) (ImagePixel i)
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
    => Convertible (Delayed l c p1) (Delayed l c p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Delayed l c p1) (Manifest p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

instance (Storable p1, Storable p2, Convertible p1 p2)
    => Convertible (Manifest p1) (Delayed () () p2) where
    safeConvert = Right . map convert
    {-# INLINE safeConvert #-}

-- | Forces an image to be in its delayed represenation. Does nothing.
delayed :: Delayed l c p -> Delayed l c p
delayed = id

-- | Forces an image to be in its manifest represenation. Does nothing.
manifest :: Manifest p -> Manifest p
manifest = id

-- -----------------------------------------------------------------------------

-- | Equivalent to @forM_ [from..to-1]@.
forRangeM_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
forRangeM_ start stop f =
    go start
  where
    go i | i >= stop = return ()
         | otherwise = f i >> go (i + 1)

{-# INLINE forRangeM_ #-}
