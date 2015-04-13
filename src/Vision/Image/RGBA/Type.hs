{-# LANGUAGE BangPatterns
           , CPP
           , RecordWildCards
           , TypeFamilies
           , TypeOperators #-}

module Vision.Image.RGBA.Type (
      RGBA, RGBAPixel (..), RGBADelayed
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

data RGBAPixel = RGBAPixel {
      rgbaRed   :: {-# UNPACK #-} !Word8, rgbaGreen :: {-# UNPACK #-} !Word8
    , rgbaBlue  :: {-# UNPACK #-} !Word8, rgbaAlpha :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

type RGBA = Manifest RGBAPixel

type RGBADelayed = Delayed RGBAPixel

instance Storable RGBAPixel where
    sizeOf _ = 4 * sizeOf (undefined :: Word8)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Word8)
    {-# INLINE alignment #-}

    peek !ptr =
        let !ptr' = castPtr ptr
        in RGBAPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                     <*> peek (ptr' `plusPtr` 2) <*> peek (ptr' `plusPtr` 3)
    {-# INLINE peek #-}

    poke !ptr RGBAPixel { .. } =
        let !ptr' = castPtr ptr
        in poke ptr'               rgbaRed   >>
           poke (ptr' `plusPtr` 1) rgbaGreen >>
           poke (ptr' `plusPtr` 2) rgbaBlue  >>
           poke (ptr' `plusPtr` 3) rgbaAlpha
    {-# INLINE poke #-}

instance Pixel RGBAPixel where
    type PixelChannel RGBAPixel    = Word8

    pixNChannels _ = 4
    {-# INLINE pixNChannels #-}

    pixIndex !(RGBAPixel r _ _ _) 0 = r
    pixIndex !(RGBAPixel _ g _ _) 1 = g
    pixIndex !(RGBAPixel _ _ b _) 2 = b
    pixIndex !(RGBAPixel _ _ _ a) _ = a
    {-# INLINE pixIndex #-}

instance Interpolable RGBAPixel where
    interpol f a b =
        let RGBAPixel aRed aGreen aBlue aAlpha = a
            RGBAPixel bRed bGreen bBlue bAlpha = b
        in RGBAPixel {
              rgbaRed  = f aRed  bRed,  rgbaGreen = f aGreen bGreen
            , rgbaBlue = f aBlue bBlue, rgbaAlpha = f aAlpha bAlpha
            }
    {-# INLINE interpol #-}
