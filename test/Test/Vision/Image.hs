{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies
           , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Vision.Image (tests) where

import Control.Applicative ((<*>), (<$>))
import Data.Vector.Storable (Storable, replicateM)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), choose)

import Vision.Image (
      MaskedImage (..), Image (..), Interpolable, FromFunction (..)
    , ImageChannel, Manifest (..), Delayed (..)
    , GreyImage, GreyPixel (..), HSVPixel, RGBAImage, RGBAPixel (..)
    , RGBADelayed, RGBImage, RGBPixel (..), InterpolMethod (..)
    , convert, resize, horizontalFlip, verticalFlip
    )
import Vision.Primitive (Z (..), (:.) (..), Size)

maxImageSize :: Int
maxImageSize = 100

instance (Arbitrary p, Storable p) => Arbitrary (Manifest p) where
    arbitrary = do
        w <- choose (1, maxImageSize)
        h <- choose (1, maxImageSize)
        vec  <- replicateM (w * h) arbitrary
        return $ Manifest (Z :. h :. w) vec

instance Arbitrary GreyPixel where
    arbitrary = GreyPixel <$> arbitrary

instance Arbitrary RGBAPixel where
    arbitrary = RGBAPixel <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary

instance Arbitrary RGBPixel where
    arbitrary = RGBPixel <$> arbitrary <*> arbitrary <*> arbitrary

tests :: [Test]
tests = [
      testGroup "Conversions identities" [
          testProperty "RGB to/from RGBA" $ propRGBRGBA
        , testProperty "RGB to/from HSV"  $ propRGBHSV
        ]
    , testGroup "Nearest-neighbor resize" [
          testProperty "Grey"
            (propImageResize :: GreyImage -> Bool)
        , testProperty "RGBA"
            (propImageResize :: RGBAImage -> Bool)
        , testProperty "RGB"
            (propImageResize :: RGBImage -> Bool)
        ]
    , testGroup "Horizontal flip is symetric" [
          testProperty "Grey"
            (propHorizontalFlip :: GreyImage -> Bool)
        , testProperty "RGBA"
            (propHorizontalFlip :: RGBAImage -> Bool)
        , testProperty "RGB"
            (propHorizontalFlip :: RGBImage -> Bool)
        ]
    , testGroup "Vertical flip is symetric" [
          testProperty "Grey"
            (propVerticalFlip :: GreyImage -> Bool)
        , testProperty "RGBA"
            (propVerticalFlip :: RGBAImage -> Bool)
        , testProperty "RGB"
            (propVerticalFlip :: RGBImage -> Bool)
        ]
    ]

-- | Tests if the conversions between RGB and RGBA images give the same images.
propRGBRGBA :: RGBImage -> Bool
propRGBRGBA img =
    let img' = convert (convert img :: RGBADelayed) :: RGBImage
    in img == img'

-- | Tests if the conversions between RGB and HSV images give the same images.
propRGBHSV :: RGBPixel -> Bool
propRGBHSV pix =
    same pix pix'
  where
    pix' = convert (convert pix :: HSVPixel) :: RGBPixel

    err = 9

    same (RGBPixel r g b) (RGBPixel r' g' b') =
        abs (r - r') + abs (g - g') + abs (b - b') <= err

-- | Tests if by increasing the size of the image by a factor of two and then
-- reducing by a factor of two give the original image.
propImageResize :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i
                   , Interpolable (ImagePixel i), Integral (ImageChannel i)
                   , Eq i)
                => i -> Bool
propImageResize img =
    img == resize' (resize' img (Z :. (h * 2) :. (w * 2))) size
  where
    size@(Z :. h :. w) = shape img

    resize' :: (Image i, FromFunction i, FromFunctionPixel i ~ ImagePixel i
               , Interpolable (ImagePixel i), Integral (ImageChannel i))
            => i -> Size -> i
    resize' img' = resize img' NearestNeighbor

-- | Tests if applying the horizontal flip twice gives the original image.
propHorizontalFlip :: (Image i, FromFunction i
                      , FromFunctionPixel i ~ ImagePixel i, Eq i) => i -> Bool
propHorizontalFlip img =
    img == horizontalFlip (delayedFlip img)
  where
    delayedFlip :: (Image i, ImagePixel i ~ p) => i -> Delayed p
    delayedFlip = horizontalFlip

-- | Tests if applying the vertical flip twice gives the original image.
propVerticalFlip :: (Image i, FromFunction i
                      , FromFunctionPixel i ~ ImagePixel i, Eq i) => i -> Bool
propVerticalFlip img =
    img == verticalFlip (delayedFlip img)
  where
    delayedFlip :: (Image i, ImagePixel i ~ p) => i -> Delayed p
    delayedFlip = verticalFlip
