{-# LANGUAGE BangPatterns, FlexibleContexts, RankNTypes, TypeFamilies #-}

module Vision.Image.Mutable (
      MutableImage (..), create
    , MutableManifest (..)
    ) where

import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.ST.Safe (ST, runST)
import Data.Vector.Storable (MVector)
import Foreign.Storable (Storable)
import Prelude hiding (read)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Vision.Image.Class (Image, ImagePixel)
import Vision.Image.Type (Manifest (..))
import Vision.Primitive (
      DIM2, Size, fromLinearIndex, toLinearIndex, shapeLength
    )

-- | Class for images which can be constructed from a mutable image.
class Image (Freezed i) => MutableImage i where
    -- | The type of the immutable version of the mutable image 'i'.
    type Freezed i

    -- | 'mShape' doesn't run in a monad as the size of a mutable image is
    -- constant.
    mShape :: i s -> Size

    -- | Creates a new mutable image of the given size. Pixels are initialized
    -- with an unknown value.
    new :: PrimMonad m => Size -> m (i (PrimState m))

    -- | Creates a new mutable image of the given size and fill it with the
    -- given value.
    new' :: PrimMonad m => Size -> ImagePixel (Freezed i) -> m (i (PrimState m))

    -- | Returns the pixel value at @Z :. y :. x@.
    read :: PrimMonad m => i (PrimState m) -> DIM2 -> m (ImagePixel (Freezed i))
    read !img !ix = img `linearRead` toLinearIndex (mShape img) ix
    {-# INLINE read #-}

    -- | Returns the pixel value as if the image was a single dimension vector
    -- (row-major representation).
    linearRead :: PrimMonad m => i (PrimState m) -> Int
               -> m (ImagePixel (Freezed i))
    linearRead !img !ix = img `read` fromLinearIndex (mShape img) ix
    {-# INLINE linearRead #-}

    -- | Overrides the value of the pixel at @Z :. y :. x@.
    write :: PrimMonad m => i (PrimState m) -> DIM2 -> ImagePixel (Freezed i)
          -> m ()
    write !img !ix !val = linearWrite img (toLinearIndex (mShape img) ix) val
    {-# INLINE write #-}

    -- | Overrides the value of the pixel at the given index as if the image was
    -- a single dimension vector (row-major representation).
    linearWrite :: PrimMonad m => i (PrimState m) -> Int
                -> ImagePixel (Freezed i) -> m ()
    linearWrite !img !ix !val = write img (fromLinearIndex (mShape img) ix) val

    -- | Returns an immutable copy of the mutable image.
    freeze :: PrimMonad m => i (PrimState m) -> m (Freezed i)

    -- | Returns the immutable version of the mutable image. The mutable image
    -- should not be modified thereafter.
    unsafeFreeze :: PrimMonad m => i (PrimState m) -> m (Freezed i)
    unsafeFreeze = freeze

    -- | Returns a mutable copy of the immutable image.
    thaw :: PrimMonad m => Freezed i -> m (i (PrimState m))

    {-# MINIMAL mShape, new, new', (read | linearRead)
              , (write | linearWrite), freeze, thaw #-}

-- | Creates an immutable image from an 'ST' action creating a mutable image.
create :: (MutableImage i) => (forall s. ST s (i s)) -> Freezed i
create action =
    runST $ do
        img <- action
        unsafeFreeze img

-- Instances -------------------------------------------------------------------

data MutableManifest p s = MutableManifest {
      mmSize   :: !Size
    , mmVector :: !(MVector s p)
    }

instance Storable p => MutableImage (MutableManifest p) where
    type Freezed (MutableManifest p) = Manifest p

    mShape = mmSize

    new  !size = do
        mvec <- MV.new (shapeLength size)
        return $! MutableManifest size mvec

    new' !size !val = do
        mvec <- MV.replicate (shapeLength size) val
        return $! MutableManifest size mvec

    linearRead  !img = MV.read  (mmVector img)
    {-# INLINE linearRead #-}

    linearWrite !img = MV.write (mmVector img)
    {-# INLINE linearWrite #-}

    freeze       !(MutableManifest size mvec) = do
        vec <- V.freeze mvec
        return $! Manifest size vec

    unsafeFreeze !(MutableManifest size mvec) = do
        vec <- V.unsafeFreeze mvec
        return $! Manifest size vec

    thaw !(Manifest size vec) = do
        mvec <- V.thaw vec
        return $! MutableManifest size mvec
