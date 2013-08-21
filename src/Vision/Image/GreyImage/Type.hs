{-# LANGUAGE TypeFamilies #-}
module Vision.Image.GreyImage.Type (GreyImage (..), GreyPixel (..)) where

import Data.Vector.Unboxed (Vector, (!), create, enumFromN, forM_)
import Data.Vector.Unboxed.Mutable (new, write)
import Data.Word

import Vision.Image.Class (Image (..), FromFunction (..))
import Vision.Image.Interpolate (Interpolable (..))

data GreyImage = GreyImage {
      greySize   :: {-# UNPACK #-} !Size
    , greyVector :: {-# UNPACK #-} !(Vector Word8)
    } deriving (Eq, Show)

newtype GreyPixel = GreyPixel Word8 deriving (Eq, Show)

instance Image GreyImage where
    type Pixel   GreyImage = GreyPixel
    type Channel GreyImage = Word8

    nChannels _ = 1
    {-# INLINE nChannels #-}

    getSize = greySize
    {-# INLINE size #-}

    fromVector = GreyImage
    {-# INLINE fromVector #-}

    toVector = greyVector
    {-# INLINE toVector #-}

    GreyImage (Size w _) vec `getPixel` Point x y =
        GreyPixel $ vec ! (y * w + x)
    {-# INLINE getPixel #-}

instance FromFunction GreyImage where
    fromFunctionLine size@(Size w h) line pixel = GreyImage size $ create $ do
        arr <- new (h * w)

        forM_ (enumFromN 0 h) $ \y -> do
            let !lineVal    = line y
                !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                let !(GreyPixel val) = pixel lineVal (Point x y)
                write arr (lineOffset + x) val

        return arr
    {-# INLINE fromFunctionLine #-}

instance Interpolable GreyImage where
    interpol _ f (GreyPixel a) (GreyPixel b) = GreyPixel $ f a b
    {-# INLINE interpol #-}

instance Eq (GreyImage r) where
    GreyImage size vec == GreyImage size' vec' = size == size' && vec == vec'
