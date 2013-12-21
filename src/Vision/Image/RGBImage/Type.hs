{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Vision.Image.RGBImage.Type (
      RGBPixel (..), RGBImage, RGBDelayed
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

data RGBPixel = RGBPixel {
      rgbRed   :: {-# UNPACK #-} !Word8, rgbGreen :: {-# UNPACK #-} !Word8
    , rgbBlue  :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

type RGBImage = Manifest RGBPixel

type RGBDelayed = Delayed RGBPixel

instance Storable RGBPixel where
    sizeOf _ = 3 * sizeOf (undefined :: Word8)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Word8)
    {-# INLINE alignment #-}

    peek ptr =
        let ptr' = castPtr ptr
        in RGBPixel <$> peek ptr'               <*> peek (ptr' `plusPtr` 1)
                    <*> peek (ptr' `plusPtr` 2)
    {-# INLINE peek #-}

    poke ptr RGBPixel { .. } =
        let ptr' = castPtr ptr
        in poke ptr'               rgbRed   >>
           poke (ptr' `plusPtr` 1) rgbGreen >>
           poke (ptr' `plusPtr` 2) rgbBlue
    {-# INLINE poke #-}

instance Pixel RGBPixel where
    type PixelChannel RGBPixel = Word8

    nChannels _ = 3
    {-# INLINE nChannels #-}

instance Interpolable RGBPixel where
    interpol f a b =
        let RGBPixel aRed aGreen aBlue = a
            RGBPixel bRed bGreen bBlue = b
        in RGBPixel {
              rgbRed  = f aRed  bRed, rgbGreen = f aGreen bGreen
            , rgbBlue = f aBlue bBlue
            }
    {-# INLINE interpol #-}

{-# SPECIALIZE crop :: RGBImage -> Rect -> RGBImage #-}
{-# SPECIALIZE resize :: RGBImage -> InterpolMethod -> Size -> RGBImage #-}
{-# SPECIALIZE horizontalFlip :: RGBImage -> RGBImage #-}
{-# SPECIALIZE verticalFlip :: RGBImage -> RGBImage #-}
