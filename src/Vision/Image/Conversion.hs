{-# LANGUAGE BangPatterns
           , MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- 'Convertible' instances for conversions between pixel types.
module Vision.Image.Conversion (Convertible (..), convert) where

import Data.Convertible (Convertible (..), ConvertResult, convert)
import Data.Word

import qualified Data.Vector.Storable as VS

import Vision.Image.Grey.Type (GreyPixel (..))
import Vision.Image.HSV.Type (HSVPixel (..))
import Vision.Image.RGBA.Type (RGBAPixel (..))
import Vision.Image.RGB.Type (RGBPixel (..))

-- to Grey ---------------------------------------------------------------------

instance Convertible GreyPixel GreyPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible HSVPixel GreyPixel where
    safeConvert pix = (safeConvert pix :: ConvertResult RGBPixel)
                      >>= safeConvert

instance Convertible RGBAPixel GreyPixel where
    safeConvert !(RGBAPixel r g b a) =
        Right $ GreyPixel $ word8 $ int (rgbToGrey r g b) * int a `quot` 255
    {-# INLINE safeConvert #-}

instance Convertible RGBPixel GreyPixel where
    safeConvert !(RGBPixel r g b) =
        Right $ GreyPixel $ rgbToGrey r g b
    {-# INLINE safeConvert #-}

-- | Converts the colors to greyscale using the human eye colors perception.
rgbToGrey :: Word8 -> Word8 -> Word8 -> Word8
rgbToGrey !r !g !b =   (redLookupTable   VS.! int r)
                     + (greenLookupTable VS.! int g)
                     + (blueLookupTable  VS.! int b)
{-# INLINE rgbToGrey #-}

redLookupTable, greenLookupTable, blueLookupTable :: VS.Vector Word8
redLookupTable   = VS.generate 256 (\val -> round $ double val * 0.299)
greenLookupTable = VS.generate 256 (\val -> round $ double val * 0.587)
blueLookupTable  = VS.generate 256 (\val -> round $ double val * 0.114)

-- to HSV ----------------------------------------------------------------------

instance Convertible HSVPixel HSVPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible GreyPixel HSVPixel where
    safeConvert pix = (safeConvert pix :: ConvertResult RGBPixel)
                      >>= safeConvert

instance Convertible RGBPixel HSVPixel where
-- Based on :
-- http://en.wikipedia.org/wiki/HSL_and_HSV#General_approach
    safeConvert !(RGBPixel r g b) =
        Right pix
      where
        (!r', !g', !b') = (int r, int g, int b)

        !pix | r >= g && r >= b = -- r == max r g b
                let !c = r' - min b' g'
                    !h = fixHue $ hue c b' g' -- Hue can be negative
                in HSVPixel (word8 h) (sat c r') r
             | g >= r && g >= b = -- g == max r g b
                let !c = g' - min r' b'
                    !h = 60 + hue c r' b'
                in HSVPixel (word8 h) (sat c g') g
             | otherwise = -- b == max r g b
                let !c = b' - min r' g'
                    !h = 120 + hue c g' r'
                in HSVPixel (word8 h) (sat c b') b

        -- Returns a value in [-30; +30].
        hue 0  _      _     = 0
        hue !c !left !right = (30 * (right - left)) `quot` c

        sat _  0 = 0
        sat !c v = word8 $ (c * 255) `quot` v

        -- Keeps the value of the hue between [0, 179].
        -- As the Hue's unit is 2°, 180 is equal to 360° and to 0°.
        fixHue !h | h < 0     = h + 180
                  | otherwise = h

instance Convertible RGBAPixel HSVPixel where
    safeConvert pix = (safeConvert pix :: ConvertResult RGBPixel)
                      >>= safeConvert

-- to RGB ----------------------------------------------------------------------

instance Convertible RGBPixel RGBPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible GreyPixel RGBPixel where
    safeConvert !(GreyPixel pix) = Right $ RGBPixel pix pix pix
    {-# INLINE safeConvert #-}

instance Convertible RGBAPixel RGBPixel where
    safeConvert !(RGBAPixel r g b a) =
        Right $ RGBPixel (withAlpha r) (withAlpha g) (withAlpha b)
      where
        !a' = int a
        withAlpha !val = word8 $ int val * a' `quot` 255
        {-# INLINE withAlpha #-}
    {-# INLINE safeConvert #-}

instance Convertible HSVPixel RGBPixel where
-- Based on :
-- http://en.wikipedia.org/wiki/HSL_and_HSV#Converting_to_RGB
    safeConvert !(HSVPixel h s v) =
        Right $! case h `quot` 30 of
                0 -> RGBPixel v                (word8 x1')      (word8 m)
                1 -> RGBPixel (word8 (x2 60))  v                (word8 m)
                2 -> RGBPixel (word8 m)        v                (word8 (x1 60))
                3 -> RGBPixel (word8 m)        (word8 (x2 120)) v
                4 -> RGBPixel (word8 (x1 120)) (word8 m)        v
                5 -> RGBPixel v                (word8 m)        (word8 (x2 180))
                _ -> error "Invalid hue value."
      where
        (!h', v') = (int h, int v)

        -- v is the major color component whereas m is the minor one.
        !m = (v' * (255 - int s)) `quot` 255

        -- Computes the remaining component by resolving the hue equation,
        -- knowing v and m. x1 is when the component is on the right of the
        -- major one, x2 when on the left.
        x1 d = (d * m - d * v' + h' * v' - h' * m + 30 * m) `quot` 30
        x1'  = (                 h' * v' - h' * m + 30 * m) `quot` 30 -- == x1 0

        x2 d = (d * v' - d * m + h' * m - h' * v' + 30 * m) `quot` 30
    {-# INLINE safeConvert #-}

-- to RGBA ---------------------------------------------------------------------

instance Convertible RGBAPixel RGBAPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

instance Convertible GreyPixel RGBAPixel where
    safeConvert !(GreyPixel pix) = Right $ RGBAPixel pix pix pix 255
    {-# INLINE safeConvert #-}

instance Convertible HSVPixel RGBAPixel where
    safeConvert pix = (safeConvert pix :: ConvertResult RGBPixel)
                      >>= safeConvert

instance Convertible RGBPixel RGBAPixel where
    safeConvert !(RGBPixel r g b) = Right $ RGBAPixel r g b 255
    {-# INLINE safeConvert #-}

-- -----------------------------------------------------------------------------

double :: Integral a => a -> Double
double = fromIntegral

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
