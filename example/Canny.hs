import Prelude hiding (filter)
import System.Environment (getArgs)

import Vision.Detector.Edge (canny)
import Vision.Image

-- Detects the edge of the image with the Canny's edge detector.
--
-- usage: ./canny input.png output.png
main :: IO ()
main = do
    [input, output] <- getArgs

    -- Loads the image. Automatically infers the format.
    io <- load Nothing input

    case io of
        Left _err -> putStrLn "Error while reading the image."
        Right img -> do
            let -- Convert the StorageImage (which can be Grey, RGB or RGBA) to
                -- a Grey image (edges are detected on greyscale images).
                grey = convert img              :: Grey

                -- Creates a Gaussian filter with a 3x3 kernel to remove small
                -- noises.
                filter = gaussianBlur 1 Nothing :: Blur GreyPixel Float
                                                        GreyPixel

                -- Applies the Gaussian filter to the grey-scale image.
                blurred = apply filter grey     :: Grey

                -- Applies the Canny's algorithm with a 5x5 Sobel kernel (radius
                -- = 2).
                edges = canny 2 256 1024 blurred  :: Grey

            -- Saves the edges image. Ignores any runtime error.
            _ <- save output edges
            return ()
