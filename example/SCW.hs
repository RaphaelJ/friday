import System.Environment (getArgs)

import Vision.Image
import Vision.Primitive (ix2)

-- Thresholds an image by applying the SCW method.
--
-- usage: ./scw .png output.png
main :: IO ()
main = do
    [input, output] <- getArgs
    io <-     either (\x -> error $ "Load failed: " ++ show x) return
          =<< load Nothing input

    let grey        = convert io                                :: Grey
        thresholded = scw (ix2 5 5) (ix2 15 15) (5 :: Double)
                          (BinaryThreshold 255 0) grey          :: Grey

    _ <- save output thresholded
    return ()
