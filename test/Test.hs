import Test.Framework (defaultMain, testGroup)

import qualified Test.Vision.Image as I
import qualified Test.Vision.Histogram as H

main :: IO ()
main = defaultMain [
      testGroup "Images"     I.tests
    , testGroup "Histograms" H.tests
    ]
