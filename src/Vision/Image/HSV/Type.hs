{-# LANGUAGE BangPatterns
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}

module Vision.Image.HSV.Type (
      HSV, HSVPixel (..), HSVDelayed
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Data.Word
import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)

import Vision.Image.Class (Pixel (..))
import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Type (Manifest, Delayed)

data HSVPixel = HSVPixel {
      hsvHue   :: {-# UNPACK #-} !Word8, hsvSat :: {-# UNPACK #-} !Word8
    , hsvValue :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

-- | 24 bits (3 * 8 bits) HSV image.
--
-- The Hue value is in [0..179], Saturation in [0..255] and Value in [0..255].
--
-- This image type is more respectful to human eye perception of colors and can
-- be converted (using 'convert') from 'RGB' images.
--
-- Uses <http://en.wikipedia.org/wiki/HSL_and_HSV> equations to convert from and
-- to RGB.
type HSV = Manifest HSVPixel

type HSVDelayed l c = Delayed l c HSVPixel

instance Storable HSVPixel where
    sizeOf _ = 3 * sizeOf (undefined :: Word8)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Word8)
    {-# INLINE alignment #-}

    peek !ptr =
        let !ptr' = castPtr ptr
        in HSVPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                    <*> peek (ptr' `plusPtr` 2)
    {-# INLINE peek #-}

    poke !ptr HSVPixel { .. } =
        let !ptr' = castPtr ptr
        in poke ptr'               hsvHue   >>
           poke (ptr' `plusPtr` 1) hsvSat   >>
           poke (ptr' `plusPtr` 2) hsvValue
    {-# INLINE poke #-}

instance Pixel HSVPixel where
    type PixelChannel HSVPixel = Word8

    pixNChannels _ = 3
    {-# INLINE pixNChannels #-}

    pixIndex !(HSVPixel h _ _) 0 = h
    pixIndex !(HSVPixel _ s _) 1 = s
    pixIndex !(HSVPixel _ _ v) _ = v
    {-# INLINE pixIndex #-}

instance Interpolable HSVPixel where
    interpol f a b =
        let HSVPixel aHue aSat aVal = a
            HSVPixel bHue bSat bVal = b
        in HSVPixel {
              hsvHue   = f aHue bHue, hsvSat = f aSat bSat
            , hsvValue = f aVal bVal
            }
    {-# INLINE interpol #-}
