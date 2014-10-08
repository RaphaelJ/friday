import Prelude hiding (filter)
import System.Environment (getArgs)

import Vision.Image

-- Applies a Gaussian blur to an image.
--
-- usage: ./gaussian_blur input.png output.png
main :: IO ()
main = do
    [input, output] <- getArgs

    -- Loads the image. Automatically infers the format.
    io <- load Nothing input

    case io of
        Left _err -> putStrLn "Error while reading the image."
        Right img -> do
            let -- Convert the StorageImage (which can be Grey, RGB or RGBA) to
                -- a Grey image (filters are currently only supported on single
                -- channel images).
                grey = convert img              :: Grey

                -- Creates a Gaussian filter with a 21x21 kernel (kernel radius
                -- of 10px).
                filter = gaussianBlur 10 Nothing :: Blur GreyPixel Float
                                                         GreyPixel

                -- Applies the filter to the grey-scale image.
                blurred = apply filter grey     :: Grey

            -- Saves the blurred image. Ignores any runtime error.
            _ <- save output blurred
            return ()
