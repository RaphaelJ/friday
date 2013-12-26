{-# LANGUAGE BangPatterns, RecordWildCards, TypeFamilies, TypeOperators #-}

module Vision.Image.HSVImage.Type (
      HSVPixel (..), HSVImage, HSVDelayed
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Word
import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)

import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Image.Type (Pixel (..), Manifest, Delayed)
import Vision.Primitive (Rect, Size)

data HSVPixel = HSVPixel {
      hsvHue   :: {-# UNPACK #-} !Word8, hsvSat :: {-# UNPACK #-} !Word8
    , hsvValue :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

-- | 24 bits (3 * 8 bits) HSV image.
-- The Hue value is in [0; 179], Saturation in [0; 255] and value in [0; 255].
--
-- This image type is more respectful to human eye perception of colors and can
-- be converted (using 'converted') from 'RGBImage's.
-- Uses
-- <http://docs.opencv.org/modules/imgproc/doc/miscellaneous_transformations.html>
-- to convert from RGB.
type HSVImage = Manifest HSVPixel

type HSVDelayed = Delayed HSVPixel

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

{-# SPECIALIZE crop :: HSVImage -> Rect -> HSVImage #-}
{-# SPECIALIZE resize :: HSVImage -> InterpolMethod -> Size -> HSVImage #-}
{-# SPECIALIZE horizontalFlip :: HSVImage -> HSVImage #-}
{-# SPECIALIZE verticalFlip :: HSVImage -> HSVImage #-}
