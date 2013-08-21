-- {-# LANGUAGE #-}
module Vision.Image.RGBImage.Type (RGBImage (..), RGBPixel (..)) where

import Data.Vector.Unboxed (Vector, (!), create, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (new, write)
import Data.Word

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Interpolate (Interpolable (..))

data RGBImage = RGBImage {
      rgbSize   :: {-# UNPACK #-} !Size
    , rgbVector :: {-# UNPACK #-} !(Vector Word8)
    } deriving (Eq, Show)

data RGBPixel = RGBPixel {
      rgbRed   :: {-# UNPACK #-} !Word8, rgbGreen :: {-# UNPACK #-} !Word8
    , rgbBlue  :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

instance Image RGBImage where
    type Pixel   RGBImage = RGBPixel
    type Channel RGBImage = Word8

    nChannels _ = 3
    {-# INLINE nChannels #-}

    getSize = rgbSize
    {-# INLINE size #-}

    fromVector = RGBImage
    {-# INLINE fromVector #-}

    toVector = rgbVector
    {-# INLINE toVector #-}

    RGBImage (Size w _) vec `getPixel` Point x y =
        let !pixOffset = (y * w + x) * 4
        in RGBpixel {
              rgbaRed   = vec ! pixOffset
            , rgbaGreen = vec ! (pixOffset + 1)
            , rgbaBlue  = vec ! (pixOffset + 2)
            }
    {-# INLINE getPixel #-}

instance FromFunction RGBAImage where
    fromFunctionLine size@(Size w h) line pixel = RGBImage size $ create $ do
        arr <- new (h * w * 3)

        i <- newSTRef 0
        forM_ (enumFromN 0 h) $ \y -> do
            let !lineVal    = line y
                !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                offset <- readSTRef i
                let !(RGBPixel r g b) = pixel lineVal (Point x y)
                    !rOffset = offset
                    !gOffset = rOffset + 1
                    !bOffset = gOffset + 1
                write arr rOffset r
                write arr gOffset g
                write arr bOffset b
                writeSTRef i (offset + 3s)

        return arr
    {-# INLINE fromFunctionLine #-}

instance Interpolable RGBImage where
    interpol _ f a b =
        let RGBPixel aRed aGreen aBlue = a
            RGBPixel bRed bGreen bBlue = b
        in RGBPixel {
              rgbRed   = f aRed   bRed, rgbGreen = f aGreen bGreen
            , rgbBlue  = f aBlue  bBlue
            }
    {-# INLINE interpol #-}
