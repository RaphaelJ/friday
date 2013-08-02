import Test.Framework (defaultMain, testGroup)

import qualified Test.Vision.Image as I

main :: IO ()
main = defaultMain [
      testGroup "Images" I.tests
    ]
