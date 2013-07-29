{-# LANGUAGE FlexibleContexts #-}
-- | Provides low level functions to manipulate images.
-- Most of these functions are symmetric to their Repa homonyms.
module Vision.Image.Function (
      applyRepa, computeP, computeS, copyS, delay, extent, inImage
    ) where

import Control.Monad (liftM)
import Data.Array.Repa (
      Array, D, DIM2, DIM3, Source, Z (..), (:.) (..)
    )
import Data.Array.Repa.Eval (Load, Target)
import qualified Data.Array.Repa as R
import Data.Word

import Vision.Image.Class (Image (..))

-- | Applies a function to the internal Repa representation.
applyRepa :: Image i => (Array r1 DIM3 (Channel i) -> Array r2 DIM3 (Channel i)) -> i r1 -> i r2
applyRepa f = fromRepa . f . toRepa
{-# INLINE applyRepa #-}

computeP :: (Image i, Load r1 DIM3 (Channel i), Source r2 (Channel i)
            , Target r2 (Channel i), Monad m)
         => i r1 -> m (i r2)
computeP img = fromRepa `liftM` R.computeP (toRepa img)

-- | Sequential computation of the pixels of the image.
computeS :: (Image i, Load r1 DIM3 (Channel i), Target r2 (Channel i))
         => i r1 -> i r2
computeS = applyRepa R.computeS
{-# INLINE computeS #-}

-- | Converts the image between manifest representations.
copyS :: (Image i, Source r1 (Channel i), Load D DIM3 (Channel i)
         , Target r2 (Channel i)) => i r1 -> i r2
copyS = applyRepa R.copyS

-- | Wraps the internal representation to a function from indices to elements.
delay :: (Image i, Source r (Channel i)) => i r -> i D
delay = applyRepa R.delay
{-# INLINE delay #-}

extent :: (Image i, Source r (Channel i)) => i r -> DIM2
extent img = let size :. _ = R.extent $ toRepa img
             in size
{-# INLINE extent #-}

-- | Checks that the point is in the image.
inImage :: (Image i, Source r (Channel i)) => DIM2 -> i r -> Bool
(Z :. y :. x) `inImage` img =
    let Z :. h :. w = extent img
       -- Casts to unsigned to removes the lower bound check.
    in word x < word w && word y < word h
{-# INLINE inImage #-}

word :: Integral a => a -> Word
word = fromIntegral
