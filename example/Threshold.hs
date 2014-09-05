import System.Environment (getArgs)

import Vision.Image

-- Thresholds an image by applying the Otsu's method.
--
-- usage: ./threshold .png output.png
main :: IO ()
main = do
    [input, output] <- getArgs
    io <-     either (\x -> error $ "Load failed: " ++ show x) return
          =<< load Nothing input

    let grey = convert io :: Grey

    _ <- save output (otsu grey)
    return ()
