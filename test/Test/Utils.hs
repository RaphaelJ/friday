
module Test.Utils
( propStorableRoundtrip
) where

import Foreign (Storable, with, peek)

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

propStorableRoundtrip :: (Eq a, Storable a) => a -> Property
propStorableRoundtrip a = monadicIO $ do
    a' <- run . with a $ peek
    assert $ a' == a

