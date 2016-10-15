import Test.Framework (defaultMain, testGroup)

import qualified Test.Vision.Image as I
import qualified Test.Vision.Histogram as H
import qualified Test.Vision.Primitive as P

main :: IO ()
main = defaultMain [
      testGroup "Images"     I.tests
    , testGroup "Histograms" H.tests
    , testGroup "Primitives" P.tests
    ]
