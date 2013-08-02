{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Vision.Image (tests) where

import Control.Applicative ((<*>), (<$>))
import Data.Array.Repa.Arbitrary (arbitraryUShaped)
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Gen, choose)

import Vision.Image (
      Image (..), Interpolable, FromFunction (..)
    , D, Source, U, Z (..), (:.) (..)
    , GreyImage, RGBAImage, RGBImage, InterpolMethod (..)
    , computeS, convert, delay, extent, resize, horizontalFlip, verticalFlip
    )

maxImageSize :: Int
maxImageSize = 5

arbitraryImage :: (Unbox (Channel i), Arbitrary (Channel i), Image i) => Int
               -> Gen (i U)
arbitraryImage nChans = do
    (w, h) <- (,) <$> choose (1, maxImageSize) <*> choose (1, maxImageSize)
    let sh = Z :. h :. w :. nChans
    fromRepa <$> arbitraryUShaped sh

instance Arbitrary (GreyImage U) where
    arbitrary = arbitraryImage 1

instance Arbitrary (RGBAImage U) where
    arbitrary = arbitraryImage 4

instance Arbitrary (RGBImage U) where
    arbitrary = arbitraryImage 3

tests :: [Test]
tests = [
      testGroup "Conversions" [
          testProperty "Grey to/from RGBA" $ propGreyRGBA
        , testProperty "Grey to/from RGB" $ propGreyRGB
        , testProperty "RGB to/from RGBA" $ propRGBRGBA
        ]
    , testGroup "Nearest-neighbor resize" [
          testProperty "Grey"
            (propImageResize . delay :: GreyImage U -> Bool)
        , testProperty "RGBA"
            (propImageResize :: RGBAImage U -> Bool)
        , testProperty "RGB" 
            (propImageResize :: RGBImage U -> Bool)
        ]
    , testGroup "Horizontal flip" [
          testProperty "Grey"
            (propHorizontalFlip . delay :: GreyImage U -> Bool)
        , testProperty "RGBA"
            (propHorizontalFlip :: RGBAImage U -> Bool)
        , testProperty "RGB"
            (propHorizontalFlip :: RGBImage U -> Bool)
        ]
    , testGroup "Vertical flip" [
          testProperty "Grey"
            (propVerticalFlip . delay :: GreyImage U -> Bool)
        , testProperty "RGBA"
            (propVerticalFlip :: RGBAImage U -> Bool)
        , testProperty "RGB"
            (propVerticalFlip :: RGBImage U -> Bool)
        ]
    ]

-- | Tests if the conversions between greyscale and RGBA images give the same
-- images.
propGreyRGBA :: GreyImage U -> Bool
propGreyRGBA img =
    let img' = convert (convert img :: RGBAImage D) :: GreyImage D
    in img == computeS img'

-- | Tests if the conversions between greyscale and RGBA images give the same
-- images.
propGreyRGB :: GreyImage U -> Bool
propGreyRGB img =
    let img' = convert (convert img :: RGBImage D) :: GreyImage D
    in img == computeS img'

-- | Tests if the conversions between RGB and RGBA images give the same images.
propRGBRGBA :: RGBImage U -> Bool
propRGBRGBA img =
    let img' = convert (convert img :: RGBAImage D) :: RGBImage D
    in img == computeS img'

-- | Tests if by increasing the size of the image by a factor of two and then
-- reducing by a factor of two give the original image.
propImageResize :: (FromFunction i, Interpolable i
                   , Source (FunctionRepr i) (Channel i)
                   , Eq (i (FunctionRepr i)))
                => i (FunctionRepr i) -> Bool
propImageResize img =
    let size@(Z :. h :. w) = extent img
    in img == resize (resize img NearestNeighbor (Z :. (h * 2) :. (w * 2)))
                     NearestNeighbor size

-- | Tests if applying the horizontal flip twice gives the original image.
propHorizontalFlip :: (FromFunction i, Source (FunctionRepr i) (Channel i)
                           , Eq (i (FunctionRepr i)))
                        => i (FunctionRepr i) -> Bool
propHorizontalFlip img = img == horizontalFlip (horizontalFlip img)

-- | Tests if applying the vertical flip twice gives the original image.
propVerticalFlip :: (FromFunction i, Source (FunctionRepr i) (Channel i)
                           , Eq (i (FunctionRepr i)))
                        => i (FunctionRepr i) -> Bool
propVerticalFlip img = img == verticalFlip (verticalFlip img)
