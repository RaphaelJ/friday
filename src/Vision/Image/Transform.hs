{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances #-}

-- | Provides high level functions to do geometric transformations on images.
--
-- Every transformation is been declared @INLINABLE@ so new image types could be
-- specialized.
module Vision.Image.Transform (
    -- * Crop
      crop
    -- * Resize
    , Resizable, TruncateInteger (..), NearestNeighbor (..), Bilinear (..)
    , resize
    -- * Flipping
    , horizontalFlip, verticalFlip
    -- * Flood fill
    , floodFill
    ) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (..))
import Data.RatioInt (RatioInt, (%))

import Vision.Image.Class (
      MaskedImage (..), Image (..), ImageChannel, (!)
    , FromFunctionPixel, FromFunction (..), FromFunctionLine (..)
    , FromFunctionLineCol (..)
    )
import Vision.Image.Interpolate (Interpolable, bilinearInterpol)
import Vision.Image.Mutable (MutableImage (..))
import Vision.Primitive (
      Z (..), (:.) (..), Point, RPoint (..), Rect (..), Size, ix2, toLinearIndex
    )

-- Crop ------------------------------------------------------------------------

-- | Maps the content of the image\'s rectangle in a new image.
crop :: (Image i1, FromFunction i2, ImagePixel i1 ~ FromFunctionPixel i2)
     => Rect -> i1 -> i2
crop !(Rect rx ry rw rh) !img =
    fromFunction (Z :. rh :. rw) $ \(Z :. y :. x) ->
        img ! ix2 (ry + y) (rx + x)
{-# INLINABLE crop #-}
-- {-# SPECIALIZE INLINE crop :: Rect -> Delayed l c p -> Delayed l c p #-}

-- Resize ----------------------------------------------------------------------

-- | Class for methods which can be used to resize an image of type 'src' to an
-- image of type 'dst'.
class Resizable method src dst where
    resize' :: method -> Size -> src -> dst

-- 'resize' is not a method of 'Resizable' because methods can't be SPECIALIZEd
-- outside their instance's declarations. By being a simple function, 'resize'
-- can be specialized for new image types.

-- | Resizes the 'Image' using the given interpolation method.
resize :: Resizable method src dst => method -> Size -> src -> dst
resize method size img = resize' method size img
{-# INLINABLE resize #-}

-- | Selects the top left pixel (fastest).
data TruncateInteger = TruncateInteger

instance ( Image src, FromFunctionLine dst Int
         , ImagePixel src ~ FromFunctionPixel dst)
         => Resizable TruncateInteger src dst where
    resize' TruncateInteger !size'@(Z :. h' :. w') !img =
        let !widthRatio  = double w / double w'
            !heightRatio = double h / double h'
            line !y' = truncate $ (double y' + 0.5) * heightRatio - 0.5
            {-# INLINE line #-}
            col  !x' = truncate $ (double x' + 0.5) * widthRatio  - 0.5
            {-# INLINE col #-}
            f !y !(Z :. _ :. x') = let !x = col x'
                                   in img ! ix2 y x
            {-# INLINE f #-}
        in fromFunctionLine size' line f
      where
        !(Z :. h :. w) = shape img
    {-# INLINE resize' #-}

-- | Selects the nearest pixel (fast).
data NearestNeighbor = NearestNeighbor

instance ( Image src, FromFunctionLine dst Int
         , ImagePixel src ~ FromFunctionPixel dst)
         => Resizable NearestNeighbor src dst where
    resize' NearestNeighbor !size'@(Z :. h' :. w') !img =
        let !widthRatio   = double w / double w'
            !heightRatio  = double h / double h'
            line !y' = round $ (double y' + 0.5) * heightRatio - 0.5
            {-# INLINE line #-}
            col  !x' = round $ (double x' + 0.5) * widthRatio  - 0.5
            {-# INLINE col #-}
            f !y !(Z :. _ :. x') = let !x = col x'
                                    in img ! ix2 y x
            {-# INLINE f #-}
        in fromFunctionLine size' line f
      where
        !(Z :. h :. w) = shape img
    {-# INLINE resize' #-}

-- | Does a double linear interpolation over the four surrounding points (slow).
--
-- See 'bilinearInterpol'.
data Bilinear = Bilinear

instance ( Image src, Interpolable (ImagePixel src), Integral (ImageChannel src)
         ,FromFunctionLineCol dst RatioInt RatioInt
         , ImagePixel src ~ FromFunctionPixel dst)
         => Resizable Bilinear src dst where
    resize' Bilinear !size'@(Z :. h' :. w') !img =
        let !widthRatio  = w % w'
            !maxWidth    = ratio (w - 1)
            !heightRatio = (h - 1) % (h' - 1)
            !maxHeight   = ratio (h - 1)

            -- Limits the interpolation to inner pixel as first and last
            -- pixels can have out of bound coordinates.
            bound !limit = min limit . max 0
            {-# INLINE bound #-}

            line !y' = bound maxHeight $   (ratio y' + 0.5) * heightRatio
                                         - 0.5
            {-# INLINE line #-}
            col  !x' = bound maxWidth  $   (ratio x' + 0.5) * widthRatio
                                         - 0.5
            {-# INLINE col #-}
            f !y !x _ = img `bilinearInterpol` RPoint x y
            {-# INLINE f #-}
        in fromFunctionLineCol size' line col f
      where
        !(Z :. h :. w) = shape img
    {-# INLINE resize' #-}

-- Flipping --------------------------------------------------------------------

-- | Reverses the image horizontally.
horizontalFlip :: ( Image i1, FromFunction i2
                  , ImagePixel i1 ~ FromFunctionPixel i2)
               => i1 -> i2
horizontalFlip !img =
    let f !(Z :. y :. x') = let !x = maxX - x'
                            in img ! ix2 y x
        {-# INLINE f #-}
    in fromFunction size f
  where
    !size@(Z :. _ :. w) = shape img
    !maxX = w - 1
{-# INLINABLE horizontalFlip #-}

-- | Reverses the image vertically.
verticalFlip :: ( Image i1, FromFunctionLine i2 Int
                , ImagePixel i1 ~ FromFunctionPixel i2)
             => i1 -> i2
verticalFlip !img =
    let line !y' = maxY - y'
        {-# INLINE line #-}
        f !y !(Z :. _ :. x) = img ! ix2 y x
        {-# INLINE f #-}
    in fromFunctionLine size line f
  where
    !size@(Z :. h :. _) = shape img
    !maxY = h - 1
{-# INLINABLE verticalFlip #-}

-- Flood fill ------------------------------------------------------------------

-- | Paints with a new value the pixels surrounding the given point of the image
-- which have the same value as the starting point.
floodFill :: (PrimMonad m, MutableImage i, Eq (ImagePixel (Freezed i)))
          => Point -> ImagePixel (Freezed i) -> i (PrimState m) -> m ()
floodFill !start !newVal !img = do
    let !linearIX = toLinearIndex size start
    val <- linearRead img linearIX
    when (val /= newVal) $ -- No reason to repaint using the same color.
        go val start linearIX
  where
    !size@(Z :. h :. w) = mShape img

    -- Runs the flood-fill algorithm from the starting point then checks the
    -- pixels at the left and at the right of the point until their value
    -- change (scanLine). Then visits the upper and lower line of neighboring
    -- pixels (visitLine).

    go !val !(Z :. y :. x) !linearIX = do
        pix <- linearRead img linearIX

        when (pix == val) $ do
            let !minLineLinearIX = linearIX - x
                !maxLineLinearIX = minLineLinearIX + w - 1

            linearWrite img linearIX newVal

            stopLeft  <- scanLine val (< minLineLinearIX) pred (linearIX - 1)
            stopRight <- scanLine val (> maxLineLinearIX) succ (linearIX + 1)

            let !from  = stopLeft  + 1
                !to    = stopRight - 1
                !xFrom = from - minLineLinearIX

            when (y > 0) $
                visitLine val (to - w) (ix2 (y - 1) xFrom) (from - w)
            when ((y + 1) < h) $
                visitLine val (to + w) (ix2 (y + 1) xFrom) (from + w)

    scanLine !val !stop !next !linearIX
        | stop linearIX = return linearIX
        | otherwise     = do
            pix <- linearRead img linearIX
            if pix == val then do linearWrite img linearIX newVal
                                  scanLine val stop next (next linearIX)
                          else return linearIX

    visitLine !val !maxLinearIX !pt@(y :. x) !linearIX
        | linearIX > maxLinearIX = return ()
        | otherwise              = do
            go val pt linearIX
            visitLine val maxLinearIX (y :. (x + 1)) (linearIX + 1)
{-# INLINABLE floodFill #-}



-- -----------------------------------------------------------------------------

double :: Integral a => a -> Double
double = fromIntegral

ratio :: Integral a => a -> RatioInt
ratio = fromIntegral
