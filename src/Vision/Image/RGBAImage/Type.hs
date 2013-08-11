{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses
           , TypeFamilies, UndecidableInstances #-}
module Vision.Image.RGBAImage.Type (RGBAImage (..), RGBAPixel (..)) where

import Data.Array.Repa (
      Array, DIM3, Source, U, Z (..)
    , (:.) (..), extent, linearIndex, fromUnboxed
    )
import Data.Array.Repa.Shape (toIndex)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (create, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (new, write)
import Data.Word

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Interpolate (Interpolable (..))

newtype RGBAImage r = RGBAImage { unRGBAImage :: Array r DIM3 Word8 }

data RGBAPixel = RGBAPixel {
      rgbaRed   :: {-# UNPACK #-} !Word8, rgbaGreen :: {-# UNPACK #-} !Word8
    , rgbaBlue  :: {-# UNPACK #-} !Word8, rgbaAlpha :: {-# UNPACK #-} !Word8
    } deriving (Show)

instance Image RGBAImage where
    type Pixel   RGBAImage = RGBAPixel
    type Channel RGBAImage = Word8

    nChannels _ = 4
    {-# INLINE nChannels #-}

    toRepa   = unRGBAImage
    {-# INLINE toRepa #-}

    fromRepa = RGBAImage
    {-# INLINE fromRepa #-}

    RGBAImage arr `getPixel` sh =
        let idx = toIndex (extent arr) (sh :. 0)
        in RGBAPixel {
              rgbaRed   = arr `linearIndex` idx
            , rgbaGreen = arr `linearIndex` (idx + 1)
            , rgbaBlue  = arr `linearIndex` (idx + 2)
            , rgbaAlpha = arr `linearIndex` (idx + 3)
            }
    {-# INLINE getPixel #-}

instance FromFunction RGBAImage where
    type FunctionRepr RGBAImage = U

    fromFunctionLine size@(Z :. h :. w) line pixel =
        RGBAImage $ fromUnboxed (size :. 4) $ create $ do
            arr <- new (h * w * 4)

            i <- newSTRef 0
            forM_ (enumFromN 0 h) $ \y -> do
                let dim1 = line (Z :. y)
                forM_ (enumFromN 0 w) $ \x -> do
                    offset <- readSTRef i
                    let RGBAPixel r g b a = pixel dim1 (Z :. y :. x)
                        rOffset = offset
                        gOffset = rOffset + 1
                        bOffset = gOffset + 1
                        aOffset = bOffset + 1
                    write arr rOffset r
                    write arr gOffset g
                    write arr bOffset b
                    write arr aOffset a
                    writeSTRef i (offset + 4)

            return arr
    {-# INLINE fromFunction #-}

instance Interpolable RGBAImage where
    interpol _ f a b =
        let RGBAPixel aRed aGreen aBlue aAlpha = a
            RGBAPixel bRed bGreen bBlue bAlpha = b
        in RGBAPixel {
              rgbaRed   = f aRed   bRed,  rgbaGreen = f aGreen bGreen
            , rgbaBlue  = f aBlue  bBlue, rgbaAlpha = f aAlpha bAlpha
            }
    {-# INLINE interpol #-}

instance Source r (Channel RGBAImage) => Eq (RGBAImage r) where
    a == b = toRepa a == toRepa b

instance Show (Array r DIM3 (Channel RGBAImage)) => Show (RGBAImage r) where
    show = show . toRepa