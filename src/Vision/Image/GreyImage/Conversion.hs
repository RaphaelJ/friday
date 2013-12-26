{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vision.Image.GreyImage.Conversion () where

import Data.Convertible (Convertible (..))
import qualified Data.Vector.Storable as V
import Data.Word

import Vision.Image.GreyImage.Type (GreyPixel (..))
import Vision.Image.RGBAImage.Type (RGBAPixel (..))
import Vision.Image.RGBImage.Type (RGBPixel (..))

instance Convertible GreyPixel GreyPixel where
    safeConvert = Right
    {-# INLINE safeConvert #-}

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
rgbToGrey !r !g !b =   (redLookupTable   V.! int r)
                     + (greenLookupTable V.! int g)
                     + (blueLookupTable  V.! int b)
{-# INLINE rgbToGrey #-}

redLookupTable, greenLookupTable, blueLookupTable :: V.Vector Word8
redLookupTable   = V.generate 256 (\val -> round $ double val * 0.299)
greenLookupTable = V.generate 256 (\val -> round $ double val * 0.587)
blueLookupTable  = V.generate 256 (\val -> round $ double val * 0.114)

double :: Integral a => a -> Double
double = fromIntegral
int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
