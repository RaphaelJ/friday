-- Requires JuicyPixels >= 3.2.7
import Vision.Image
import Vision.Image.Contour
import Vision.Image.JuicyPixels (toFridayRGBA, toJuicyGrey)

import Codec.Picture (readImage, convertRGBA8, savePngImage, DynamicImage(ImageY8))
import System.Environment (getArgs)

main = do
  [i] <- getArgs
  img <- (toFridayRGBA . convertRGBA8) <$> readImage i
  let cs = contours (otsu (BinaryThreshold 255 0) (convert img :: Grey) :: Grey)
      x  = drawContours cs AllOutlines (allContourIds cs) (shape img)
  savePngImage "contours.png" (ImageY8 $ toJuicyGrey x)
