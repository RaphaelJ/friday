{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Vision.Primitive
( tests
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<*>), (<$>))
#endif

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Property)
import Test.Utils (propStorableRoundtrip)

import Vision.Primitive

instance Arbitrary Z where
    arbitrary = return Z

instance (Arbitrary t, Arbitrary h) => Arbitrary (t :. h) where
    arbitrary = (:.) <$> arbitrary <*> arbitrary

tests :: [Test]
tests =
    [ testGroup "Storable can roundtrip"
        [ testProperty "DIM0" $ (propStorableRoundtrip :: DIM0 -> Property)
        , testProperty "DIM1" $ (propStorableRoundtrip :: DIM1 -> Property)
        , testProperty "DIM2" $ (propStorableRoundtrip :: DIM2 -> Property)
        , testProperty "DIM3" $ (propStorableRoundtrip :: DIM3 -> Property)
        , testProperty "DIM4" $ (propStorableRoundtrip :: DIM4 -> Property)
        , testProperty "DIM5" $ (propStorableRoundtrip :: DIM5 -> Property)
        , testProperty "DIM6" $ (propStorableRoundtrip :: DIM6 -> Property)
        , testProperty "DIM7" $ (propStorableRoundtrip :: DIM7 -> Property)
        , testProperty "DIM8" $ (propStorableRoundtrip :: DIM8 -> Property)
        , testProperty "DIM9" $ (propStorableRoundtrip :: DIM9 -> Property)
        ]
    ]

