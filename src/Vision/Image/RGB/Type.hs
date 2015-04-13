{-# LANGUAGE BangPatterns
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}

module Vision.Image.RGB.Type (
      RGB, RGBPixel (..), RGBDelayed
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

data RGBPixel = RGBPixel {
      rgbRed   :: {-# UNPACK #-} !Word8, rgbGreen :: {-# UNPACK #-} !Word8
    , rgbBlue  :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

type RGB = Manifest RGBPixel

type RGBDelayed = Delayed RGBPixel

instance Storable RGBPixel where
    sizeOf _ = 3 * sizeOf (undefined :: Word8)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Word8)
    {-# INLINE alignment #-}

    peek !ptr =
        let !ptr' = castPtr ptr
        in RGBPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                    <*> peek (ptr' `plusPtr` 2)
    {-# INLINE peek #-}

    poke !ptr RGBPixel { .. } =
        let !ptr' = castPtr ptr
        in poke ptr'               rgbRed   >>
           poke (ptr' `plusPtr` 1) rgbGreen >>
           poke (ptr' `plusPtr` 2) rgbBlue
    {-# INLINE poke #-}

instance Pixel RGBPixel where
    type PixelChannel RGBPixel = Word8

    pixNChannels _ = 3
    {-# INLINE pixNChannels #-}

    pixIndex !(RGBPixel r _ _) 0 = r
    pixIndex !(RGBPixel _ g _) 1 = g
    pixIndex !(RGBPixel _ _ b) _ = b
    {-# INLINE pixIndex #-}

instance Interpolable RGBPixel where
    interpol f a b =
        let RGBPixel aRed aGreen aBlue = a
            RGBPixel bRed bGreen bBlue = b
        in RGBPixel {
              rgbRed  = f aRed  bRed, rgbGreen = f aGreen bGreen
            , rgbBlue = f aBlue bBlue
            }
    {-# INLINE interpol #-}
