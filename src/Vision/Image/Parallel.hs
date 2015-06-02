{-# LANGUAGE BangPatterns
           , FlexibleContexts #-}

module Vision.Image.Parallel (computeP) where

import Control.Concurrent (
    forkIO, getNumCapabilities, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.ST (ST, stToIO)
import Data.Vector (enumFromN, forM, forM_)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafePerformIO)

import Vision.Image.Class (MaskedImage (..), Image (..), (!))
import Vision.Image.Type (Manifest (..))
import Vision.Image.Mutable (MutableManifest, linearWrite, new, unsafeFreeze)
import Vision.Primitive (Z (..), (:.) (..), ix2)


-- | Parallel version of 'compute'.
--
-- Computes the value of an image into a manifest representation in parallel.
--
-- The monad ensures that the image is fully evaluated before continuing.
computeP :: (Monad m, Image i, Storable (ImagePixel i))
        => i -> m (Manifest (ImagePixel i))
computeP !src =
    return $! unsafePerformIO $ do
        dst <- stToIO newManifest

        -- Forks 'nCapabilities' threads.
        childs <- forM (enumFromN 0 nCapabilities) $ \c -> do
            child <- newEmptyMVar

            _ <- forkIO $ do
                let nLines | c == 0    = nLinesPerThread + remain
                           | otherwise = nLinesPerThread

                stToIO $ fillFromN dst (c * nLinesPerThread) nLines

                -- Sends a signal to the main thread.
                putMVar child ()

            return child

        -- Waits for all threads to finish.
        forM_ childs takeMVar

        stToIO $ unsafeFreeze dst
  where
    !size@(Z :. h :. w) = shape src

    !nCapabilities = unsafePerformIO getNumCapabilities

    !(nLinesPerThread, remain) = h `quotRem` nCapabilities

    -- Computes 'n' lines starting at 'from' of the image.
    fillFromN !dst !from !n =
        forM_ (enumFromN from n) $ \y -> do
            let !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                let !offset = lineOffset + x
                    !val    = src ! (ix2 y x)
                linearWrite dst offset val

    newManifest :: Storable p => ST s (MutableManifest p s)
    newManifest = new size
{-# INLINE computeP #-}