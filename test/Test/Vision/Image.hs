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
      Image (..), Interpolable, FromFunction (..), Manifest (..), Delayed (..)
    , Size (..), GreyImage, GreyPixel (..), RGBAImage, RGBAPixel (..)
    , RGBADelayed, RGBImage, RGBDelayed, RGBPixel (..), InterpolMethod (..)
    , convert, resize, horizontalFlip, verticalFlip
    )

maxImageSize :: Int
maxImageSize = 100

instance (Arbitrary p, Storable p) => Arbitrary (Manifest p) where
    arbitrary = do
        size <- Size <$> choose (1, maxImageSize) <*> choose (1, maxImageSize)
        vect <- replicateM (sWidth size * sHeight size) arbitrary
        return $ Manifest size vect

instance Arbitrary GreyPixel where
    arbitrary = GreyPixel <$> arbitrary

instance Arbitrary RGBAPixel where
    arbitrary = RGBAPixel <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary

instance Arbitrary RGBPixel where
    arbitrary = RGBPixel <$> arbitrary <*> arbitrary <*> arbitrary

tests :: [Test]
tests = [
      testGroup "Conversions" [
          testProperty "Grey to/from RGBA" $ propGreyRGBA
        , testProperty "Grey to/from RGB" $ propGreyRGB
        , testProperty "RGB to/from RGBA" $ propRGBRGBA
        ]
    , testGroup "Nearest-neighbor resize" [
          testProperty "Grey"
            (propImageResize :: GreyImage -> Bool)
        , testProperty "RGBA"
            (propImageResize :: RGBAImage -> Bool)
        , testProperty "RGB"
            (propImageResize :: RGBImage -> Bool)
        ]
    , testGroup "Horizontal flip" [
          testProperty "Grey"
            (propHorizontalFlip :: GreyImage -> Bool)
        , testProperty "RGBA"
            (propHorizontalFlip :: RGBAImage -> Bool)
        , testProperty "RGB"
            (propHorizontalFlip :: RGBImage -> Bool)
        ]
    , testGroup "Vertical flip" [
          testProperty "Grey"
            (propVerticalFlip :: GreyImage -> Bool)
        , testProperty "RGBA"
            (propVerticalFlip :: RGBAImage -> Bool)
        , testProperty "RGB"
            (propVerticalFlip :: RGBImage -> Bool)
        ]
    ]

-- | Tests if the conversions between greyscale and RGBA images give the same
-- images.
propGreyRGBA :: GreyImage -> Bool
propGreyRGBA img =
    let img' = convert (convert img :: RGBADelayed) :: GreyImage
    in img == img'

-- | Tests if the conversions between greyscale and RGBA images give the same
-- images.
propGreyRGB :: GreyImage -> Bool
propGreyRGB img =
    let img' = convert (convert img :: RGBDelayed) :: GreyImage
    in img == img'

-- | Tests if the conversions between RGB and RGBA images give the same images.
propRGBRGBA :: RGBImage -> Bool
propRGBRGBA img =
    let img' = convert (convert img :: RGBADelayed) :: RGBImage
    in img == img'

-- | Tests if by increasing the size of the image by a factor of two and then
-- reducing by a factor of two give the original image.
propImageResize :: (FromFunction i, Interpolable (ImagePixel i), Eq i)
                => i -> Bool
propImageResize img =
    img == resize' (resize' img (Size (w * 2) (h * 2))) size
  where
    size@(Size w h) = getSize img

    resize' :: (FromFunction i, Interpolable (ImagePixel i)) => i -> Size -> i
    resize' img' = resize img' NearestNeighbor

-- | Tests if applying the horizontal flip twice gives the original image.
propHorizontalFlip :: (FromFunction i, Eq i) => i -> Bool
propHorizontalFlip img =
    img == horizontalFlip (delayedFlip img)
  where
    delayedFlip :: (Image i, ImagePixel i ~ p) => i -> Delayed p
    delayedFlip = horizontalFlip

-- | Tests if applying the vertical flip twice gives the original image.
propVerticalFlip :: (FromFunction i, Eq i) => i -> Bool
propVerticalFlip img =
    img == verticalFlip (delayedFlip img)
  where
    delayedFlip :: (Image i, ImagePixel i ~ p) => i -> Delayed p
    delayedFlip = verticalFlip
