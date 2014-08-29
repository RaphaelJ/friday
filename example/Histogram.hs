import Data.Int
import System.Environment (getArgs)
import Text.Printf

import Vision.Histogram
import Vision.Image
import Vision.Primitive

-- Compares two images by their HSV histograms.
--
-- usage: ./histogram input1.png input2.png
main :: IO ()
main = do
    [input1, input2] <- getArgs

    -- Loads the images. Automatically infers the format.
    io1 <- load Nothing input1
    io2 <- load Nothing input2

    case (io1, io2) of
        (Right img1, Right img2) -> do
            let rgb1 = convert img1 :: RGB
                rgb2 = convert img2 :: RGB

                -- Converts both images to the HSV color space as it gives
                -- better results when comparing colors.
                hsv1 = convert rgb1 :: HSV
                hsv2 = convert rgb2 :: HSV

                -- Computes a small histogram so two colors which are similar
                -- will be in the same bin.
                histSize = Just $ ix3 10 5 5

                hist1 = histogram histSize hsv1 :: Histogram DIM3 Int32
                hist2 = histogram histSize hsv2 :: Histogram DIM3 Int32

                -- Normalizes both histograms as the number of pixels in the two
                -- images could be different.
                hist1' = normalize 100 hist1    :: Histogram DIM3 Double
                hist2' = normalize 100 hist2    :: Histogram DIM3 Double

                intersec = compareIntersect hist1' hist2'

            printf "The two images share %.2f%% of their colors.\n" intersec

        _ -> putStrLn "Error while reading the images."
