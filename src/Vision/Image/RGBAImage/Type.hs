-- {-# LANGUAGE  #-}
module Vision.Image.RGBAImage.Type (RGBAImage (..), RGBAPixel (..)) where

import Data.Vector.Unboxed (Vector, (!), create, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (new, write)
import Data.Word

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Interpolate (Interpolable (..))

data RGBAImage = RGBAImage {
      rgbaSize   :: {-# UNPACK #-} !Size
    , rgbaVector :: {-# UNPACK #-} !(Vector Word8)
    } deriving (Eq, Show)

data RGBAPixel = RGBAPixel {
      rgbaRed   :: {-# UNPACK #-} !Word8, rgbaGreen :: {-# UNPACK #-} !Word8
    , rgbaBlue  :: {-# UNPACK #-} !Word8, rgbaAlpha :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

instance Image RGBAImage where
    type Pixel   RGBAImage = RGBAPixel
    type Channel RGBAImage = Word8

    nChannels _ = 4
    {-# INLINE nChannels #-}

    getSize = rgbaSize
    {-# INLINE size #-}

    fromVector = RGBAImage
    {-# INLINE fromVector #-}

    toVector = rgbaVector
    {-# INLINE toVector #-}

    RGBAImage (Size w _) vec `getPixel` Point x y =
        let !pixOffset = (y * w + x) * 4
        in RGBAPixel {
              rgbaRed   = vec ! pixOffset
            , rgbaGreen = vec ! (pixOffset + 1)
            , rgbaBlue  = vec ! (pixOffset + 2)
            , rgbaAlpha = vec ! (pixOffset + 3)
            }
    {-# INLINE getPixel #-}

instance FromFunction RGBAImage where
    fromFunctionLine size@(Size w h) line pixel = RGBAImage size $ create $ do
        arr <- new (h * w * 4)

        i <- newSTRef 0
        forM_ (enumFromN 0 h) $ \y -> do
            let !lineVal    = line y
                !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                offset <- readSTRef i
                let !(RGBAPixel r g b a) = pixel lineVal (Point x y)
                    !rOffset = offset
                    !gOffset = rOffset + 1
                    !bOffset = gOffset + 1
                    !aOffset = bOffset + 1
                write arr rOffset r
                write arr gOffset g
                write arr bOffset b
                write arr aOffset a
                writeSTRef i (offset + 4)

        return arr
    {-# INLINE fromFunctionLine #-}

instance Interpolable RGBAImage where
    interpol _ f a b =
        let RGBAPixel aRed aGreen aBlue aAlpha = a
            RGBAPixel bRed bGreen bBlue bAlpha = b
        in RGBAPixel {
              rgbaRed   = f aRed   bRed,  rgbaGreen = f aGreen bGreen
            , rgbaBlue  = f aBlue  bBlue, rgbaAlpha = f aAlpha bAlpha
            }
    {-# INLINE interpol #-}
