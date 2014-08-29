import System.Environment (getArgs)

import Vision.Image
import Vision.Primitive (ix2)

-- Resizes the input image to a square of 250x250 pixels.
--
-- usage: ./resize_image input.png output.png
main :: IO ()
main = do
    [input, output] <- getArgs

    -- Loads the image. Automatically infers the format.
    io <- load Nothing input

    case io of
        Left _err -> putStrLn "Error while reading the image."
        Right img -> do
            let -- Convert the StorageImage (which can be Grey, RGB or RGBA) to
                -- an RGB image.
                rgb = convert img                             :: RGB

                -- Resizes the RGB image to 250x250 pixels.
                miniature = resize Bilinear (ix2 250 250) rgb :: RGB

            -- Saves the miniature. Ignores any runtime error.
            _ <- save output miniature
            return ()
