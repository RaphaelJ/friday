{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses
           , TypeFamilies, UndecidableInstances #-}
module Vision.Image.RGBImage.Type (RGBImage (..), RGBPixel (..)) where

import Data.Array.Repa (
      Array, DIM3, Source, U, Z (..), (:.) (..)
    , extent, linearIndex, fromUnboxed
    )
import Data.Array.Repa.Shape (toIndex)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (create, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (new, write)
import Data.Word

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Interpolate (Interpolable (..))

newtype RGBImage  r = RGBImage  { unRGBImage  :: Array r DIM3 Word8 }

data RGBPixel = RGBPixel {
      rgbRed   :: {-# UNPACK #-} !Word8, rgbGreen :: {-# UNPACK #-} !Word8
    , rgbBlue  :: {-# UNPACK #-} !Word8
    } deriving (Show)

instance Image RGBImage where
    type Pixel   RGBImage = RGBPixel
    type Channel RGBImage = Word8

    nChannels _ = 3
    {-# INLINE nChannels #-}

    toRepa   = unRGBImage
    {-# INLINE toRepa #-}

    fromRepa = RGBImage
    {-# INLINE fromRepa #-}

    RGBImage arr `getPixel` sh =
        let idx = toIndex (extent arr) (sh :. 0)
        in RGBPixel {
              rgbRed   = arr `linearIndex` idx
            , rgbGreen = arr `linearIndex` (idx + 1)
            , rgbBlue  = arr `linearIndex` (idx + 2)
            }
    {-# INLINE getPixel #-}

instance FromFunction RGBImage where
    type FunctionRepr RGBImage = U

    fromFunctionLine size@(Z :. h :. w) line pixel =
        RGBImage $ fromUnboxed (size :. 3) $ create $ do
            arr <- new (h * w * 3)

            i <- newSTRef 0
            forM_ (enumFromN 0 h) $ \y -> do
                let dim1 = line (Z :. y)
                forM_ (enumFromN 0 w) $ \x -> do
                    offset <- readSTRef i
                    let RGBPixel r g b = pixel dim1 (Z :. y :. x)
                        rOffset = offset
                        gOffset = rOffset + 1
                        bOffset = gOffset + 1
                    write arr rOffset r
                    write arr gOffset g
                    write arr bOffset b
                    writeSTRef i (offset + 3)

            return arr
    {-# INLINE fromFunction #-}

instance Interpolable RGBImage where
    interpol _ f a b =
        let RGBPixel aRed aGreen aBlue = a
            RGBPixel bRed bGreen bBlue = b
        in RGBPixel {
              rgbRed   = f aRed   bRed, rgbGreen = f aGreen bGreen
            , rgbBlue  = f aBlue  bBlue
            }
    {-# INLINE interpol #-}

instance Source r (Channel RGBImage) => Eq (RGBImage r) where
    a == b = toRepa a == toRepa b

instance Show (Array r DIM3 (Channel RGBImage)) => Show (RGBImage r) where
    show = show . toRepa
