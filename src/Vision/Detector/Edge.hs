{-# LANGUAGE BangPatterns, MultiWayIf #-}

module Vision.Detector.Edge (canny) where

import Control.Monad (when)
import Control.Monad.ST.Safe (ST)
import Data.Int
import Data.RatioInt (RatioInt, (%))
import Data.Vector.Storable (enumFromN, forM_)
import Data.Word

import Vision.Image (
      Manifest, MutableManifest, GreyPixel, GreyImage
    , SeparableFilter, Derivative (..)
    , shape, index, linearIndex, fromFunction, bilinearInterpol
    , create, new', linearRead, linearWrite
    , apply, gaussianBlur, scharr
    )
import Vision.Primitive (Z (..), (:.) (..), RPoint (..), inShape, ix2)

data EdgeDirection = NorthSouth         -- ^ |
                   | WestEast           -- ^ ―
                   | NorthEastSouthWest -- ^ /
                   | NorthWestSouthEast -- ^ \

-- Detects edges using the Canny's algorithm.
-- See <http://en.wikipedia.org/wiki/Canny_edge_detector> for details.
-- canny :: (Image i, ImagePixel i ~ GreyPixel) => i
canny :: GreyImage -> Int -> Maybe Float -> Int32 -> Int32 -> GreyImage
canny !img !gaussRadius !gaussSig !lowThres !highThres =
    create $ do
        edges <- new' size 0 :: ST s (MutableManifest GreyPixel s)
        forM_ (enumFromN 0 h) $ \y -> do
            let !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                visitPoint edges x y (lineOffset + x) highThres'
        return edges
  where
    !size@(Z :. h :. w) = shape img

    (!h', !w') = (ratio (h - 1), ratio (w - 1))

    -- Squares both thresholds as they will be compared to 'dxy' which contains
    -- squared gradient magnitudes.
    (!lowThres', !highThres') = (square lowThres, square highThres)

    blurred        :: GreyImage
    !blurred        = img `apply` gaussianBlur gaussRadius gaussSig

    dx, dy :: Manifest Int16
    !dx = img `apply` scharr DerivativeX
    !dy = img `apply` scharr DerivativeY

    -- Magnitude of the gradient, squared.
    dxy :: Manifest Int32
    !dxy = fromFunction size $ \pt ->
                  square (fromIntegral $ dx `index` pt)
                + square (fromIntegral $ dy `index` pt)

    visitPoint !edges !x !y !offset !thres = do
        val <- linearRead edges offset

        when (val == 0) $ do
            let !ptDxy  = fromIntegral $ dxy `linearIndex` offset
                ptDx    = fromIntegral $ dx  `linearIndex` offset
                ptDy    = fromIntegral $ dy  `linearIndex` offset

            -- When the current pixel has a greater magnitude than the high
            -- threshold and is a local maxima, considers it as the starting
            -- point of an edge. Tries to draw the remaining of the edge
            -- using the low threshold and by following the edge direction.

            when (ptDxy > thres && isMaxima y x ptDx ptDy ptDxy) $ do
                linearWrite edges offset 255
                visitNeighbour edges x y ptDx ptDy

    visitNeighbour !edges !x !y !ptDx !ptDy = do
        let (x1, y1, x2, y2) =
                case gradientDirection ptDx ptDy of
                    NorthSouth         -> (x - 1, y,     x + 1, y    )
                    WestEast           -> (x,     y - 1, x,     y + 1)
                    NorthEastSouthWest -> (x - 1, y + 1, x + 1, y - 1)
                    NorthWestSouthEast -> (x - 1, y - 1, y + 1, x + 1)

        when (inShape size (ix2 y1 x1)) $
            visitPoint edges x1 y1 (y1 * w + x1) lowThres'

        when (inShape size (ix2 y2 x2)) $
            visitPoint edges x2 y2 (y2 * w + x2) lowThres'

    isMaxima !x !y !ptDx !ptDy !ptDxy =
        let !ptDxy' = round (sqrt $ fromIntegral ptDxy :: Double)
            ptDx' = ptDx % ptDxy'
            ptDy' = ptDy % ptDxy'
            x'    = ratio x
            y'    = ratio y
            next  = RPoint (x' + ptDx') (y' + ptDy')
            prec  = RPoint (x' - ptDx') (y' - ptDy')
        in tryCompare ptDxy next && tryCompare ptDxy prec

    tryCompare !ptDxy !pt@(RPoint x y)
        | x >= 0 && y >= 0 && x < w' && y < h' =
            fromIntegral (dxy `bilinearInterpol` pt) < ptDxy
        | otherwise = True

    gradientDirection ptDx ptDy =
        let !angle = atan2 (double ptDy) (double ptDx)
        in if angle >= 0 then if | angle >  7 * pi8 -> WestEast
                                 | angle >  5 * pi8 -> NorthWestSouthEast
                                 | angle >  3 * pi8 -> NorthSouth
                                 | otherwise        -> NorthEastSouthWest
                         else if | angle < -7 * pi8 -> WestEast
                                 | angle < -5 * pi8 -> NorthEastSouthWest
                                 | angle < -3 * pi8 -> NorthSouth
                                 | otherwise        -> NorthWestSouthEast

    !pi8 = pi / 8

--         if ptDx > ptDy
--            then 
--            
--            
--         if ptDx >= 0
--            then -- Gradient goes to the right.
--                 if ptDy >= 0
--                     then -- Gradient goes up.
--                         
--                     else -- Gradient goes down.
--            else -- Gradient goes to the left.

square :: Num a => a -> a
square a = a * a

double :: Integral a => a -> Double
double = fromIntegral

ratio :: Integral a => a -> RatioInt
ratio = fromIntegral

word :: Integral a => a -> Word
word = fromIntegral
