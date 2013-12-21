{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Vision.Image.RGBAImage.Type (
      RGBAPixel (..), RGBAImage, RGBADelayed
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Word
import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)

import Vision.Image.Interpolate (Interpolable (..))
import Vision.Image.Primitive (Rect, Size)
import Vision.Image.Transform (
      InterpolMethod, crop, resize, horizontalFlip, verticalFlip
    )
import Vision.Image.Type (Pixel (..), Manifest, Delayed)

data RGBAPixel = RGBAPixel {
      rgbaRed   :: {-# UNPACK #-} !Word8, rgbaGreen :: {-# UNPACK #-} !Word8
    , rgbaBlue  :: {-# UNPACK #-} !Word8, rgbaAlpha :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

type RGBAImage = Manifest RGBAPixel

type RGBADelayed = Delayed RGBAPixel

instance Storable RGBAPixel where
    sizeOf _ = 4 * sizeOf (undefined :: Word8)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Word8)
    {-# INLINE alignment #-}

    peek ptr =
        let ptr' = castPtr ptr
        in RGBAPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                     <*> peek (ptr' `plusPtr` 2) <*> peek (ptr' `plusPtr` 3)
    {-# INLINE peek #-}

    poke ptr RGBAPixel { .. } =
        let ptr' = castPtr ptr
        in poke ptr'               rgbaRed   >>
           poke (ptr' `plusPtr` 1) rgbaGreen >>
           poke (ptr' `plusPtr` 2) rgbaBlue  >>
           poke (ptr' `plusPtr` 3) rgbaAlpha
    {-# INLINE poke #-}

instance Pixel RGBAPixel where
    type PixelChannel RGBAPixel = Word8

    nChannels _ = 4
    {-# INLINE nChannels #-}

instance Interpolable RGBAPixel where
    interpol f a b =
        let RGBAPixel aRed aGreen aBlue aAlpha = a
            RGBAPixel bRed bGreen bBlue bAlpha = b
        in RGBAPixel {
              rgbaRed  = f aRed  bRed,  rgbaGreen = f aGreen bGreen
            , rgbaBlue = f aBlue bBlue, rgbaAlpha = f aAlpha bAlpha
            }
    {-# INLINE interpol #-}

{-# SPECIALIZE crop :: RGBAImage -> Rect -> RGBAImage #-}
{-# SPECIALIZE resize :: RGBAImage -> InterpolMethod -> Size -> RGBAImage #-}
{-# SPECIALIZE horizontalFlip :: RGBAImage -> RGBAImage #-}
{-# SPECIALIZE verticalFlip :: RGBAImage -> RGBAImage #-}
