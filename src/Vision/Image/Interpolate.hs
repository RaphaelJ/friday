{-# LANGUAGE BangPatterns, FlexibleContexts #-}

-- | Provides a way to estimate the value of a pixel at rational coordinates
-- using a linear interpolation.
module Vision.Image.Interpolate (
      Interpolable (..), bilinearInterpol
    ) where

import Data.RatioInt (RatioInt, denominator, numerator)

import Vision.Image.Type (Pixel (..), Image (..), ImagePixel)
import Vision.Primitive (RPoint (..), ix2)

-- | Provides a way to apply the interpolation to every component of a pixel.
class Integral (PixelChannel p) => Interpolable p where
    -- | Given a function which interpolates two points over a single channel,
    -- returns a function which interpolates two points over every channel of
    -- two pixels.
    interpol :: (PixelChannel p -> PixelChannel p -> PixelChannel p)
             -> p -> p -> p

-- | Uses a bilinear interpolation to find the value of the pixel at the
-- rational coordinates.
--
-- Estimates the value of a rational point @p@ using @a@, @b@, @c@ and @d@ :
--
-- >       x1       x2
-- >
-- > y1    a ------ b
-- >       -        -
-- >       -  p     -
-- >       -        -
-- > y2    c ------ d
bilinearInterpol :: (Image i, Interpolable (ImagePixel i))
                 => i -> RPoint -> ImagePixel i
img `bilinearInterpol` RPoint x y
    | not integralX && not integralY =
        let (!x1, !deltaX1) = properFraction x
            (!y1, !deltaY1) = properFraction y
            !x2 = x1 + 1
            !y2 = y1 + 1
            !a = img `index` ix2 y1 x1
            !b = img `index` ix2 y1 x2
            !c = img `index` ix2 y2 x1
            !d = img `index` ix2 y2 x2

            -- Computes the relative distance to the four points.
            !deltaX2 = compl deltaX1
            !deltaY2 = compl deltaY1

            !interpolX1 = interpol (interpolChannel deltaX1 deltaX2) a b
            !interpolX2 = interpol (interpolChannel deltaX1 deltaX2) c d
        in interpol (interpolChannel deltaY1 deltaY2) interpolX1 interpolX2
    | not integralX =
        let (!x1, !deltaX1) = properFraction x
            !y1     = truncate y
            !x2     = x1 + 1
            !a = img `index` ix2 y1 x1
            !b = img `index` ix2 y1 x2
            !deltaX2 = compl deltaX1
        in interpol (interpolChannel deltaX1 deltaX2) a b
    | not integralY =
        let !x1     = truncate x
            (!y1, !deltaY1) = properFraction y
            !y2     = y1 + 1
            !a      = img `index` ix2 y1 x1
            !c      = img `index` ix2 y2 x1
            !deltaY2 = compl deltaY1
        in interpol (interpolChannel deltaY1 deltaY2) a c
    | otherwise = img `index` ix2 (numerator y) (numerator x)
--     | x1 >= 0 && y1 >= 0 && (Z :. y2 :. x2) `inImage` img =
--         error "Invalid index"
  where
    integralX = denominator x == 1
    integralY = denominator y == 1

    -- compl delta = 1 - delta
    compl delta = delta {
          numerator = denominator delta - numerator delta
        }
    {-# INLINE compl #-}

    -- Interpolates the value of a single channel given its two surrounding
    -- points.
    interpolChannel deltaA deltaB chanA chanB = truncate $
--         (fromIntegral chanA) * deltaB + (fromIntegral chanB) * deltaA
--           deltaB { numerator = int chanA * numerator deltaB }
--         + deltaA { numerator = int chanB * numerator deltaA }
        deltaA {
              numerator = int chanA * numerator deltaB
                        + int chanB * numerator deltaA
            }
    {-# INLINE interpolChannel #-}
{-# INLINE bilinearInterpol #-}

int :: Integral a => a -> Int
int = fromIntegral
