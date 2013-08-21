{-# LANGUAGE TypeFamilies #-}
module Vision.Image.Class (Image (..), FromFunction (..), applyVector) where

import Data.Vector.Unboxed (Vector)

import Vision.Image.Primitive (Point (..), Size)

-- | Class for image which are represented as a 'Vector'.
-- Vectors encode the image in the row-column-channel order.
class Image i where
    -- | Determines the number of channels and the type of each pixel of the
    -- image.
    type Pixel i

    -- | Determines the type of each channel of the pixel.
    type Channel i

    -- | Determines the number of channels of each pixel.
    -- This must be equal to the size of the last dimension of the array.
    nChannels :: i -> Int

    getSize :: i -> Size

    -- | Boxes the internal 'Vector' representation of the image.
    fromVector :: Size -> Vector (Channel i) -> i

    -- | Unboxes the internal 'Vector' representation of the image.
    toVector   :: i -> Vector (Channel i)

    -- | Returns the value of every channel at the given coordinates.
    getPixel :: i -> Point -> Pixel i

-- | Defines how an image can be generated from a function.
-- Minimal definition is 'fromFunction' or 'fromFunctionLine'.
class Image i => FromFunction i where
    -- | Calls the given function for each pixel of the constructed image.
    fromFunction :: Size -> (Point -> Pixel i) -> i
    fromFunction size pixel =
        fromFunctionLine size (const ()) (\_ pt -> pixel pt)
    {-# INLINE fromFunction #-}

    -- | Calls the first function at each line of the image then calls the
    -- second function for each pixel of the constructed image, giving the
    -- value which has been computed at each line.
    fromFunctionLine :: Size -> (Int -> a) -> (a -> Point -> Pixel i) -> i
    fromFunctionLine size line pixel =
        fromFunction size (\pt@(Point _ y) -> pixel (line y) pt)
    {-# INLINE fromFunctionLine #-}

-- | Applies a function to the internal 'Vector' representation.
applyVector :: Image i => (Vector (Channel i) -> Vector (Channel i)) -> i -> i
applyVector f img= fromVector (getSize img) $ f $ toVector img
{-# INLINE applyVector #-}
