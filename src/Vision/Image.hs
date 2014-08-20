-- | Images are manipulated by their 'Image' and 'MaskedImage' type-class
-- instances.
--
-- The 'Manifest' representation uses an internal 'Vector' to represent the
-- image whereas the 'Delayed' representation uses a function to generate
-- pixels. Most transformation functions are generic to both representations in
-- the way they apply to any type which implements the type-classes.
--
-- The 'Delayed' image should be used as intermediate representations of
-- complex image transformations.
--
-- The following example reads an image from a file, applies a composition of
-- transformations to create a centred and squared miniature and then writes
-- the result to a file:
--
-- @
-- import Vision.Image
-- import Vision.Primitive (Z (..), (:.) (..), ix2)
--
-- main = do
--      io <- 'load' \"image.png\"
--
--      case io of
--          Left _err -> putStrLn "Error while reading the image."
--          Right img -> do
--              let rgb = 'convert' io :: 'RGB'
--                  Z :. h :. w  = 'shape' rgb
--                  miniature | w > h =
--                      resizeSquare $ 'crop' ('Rect' ((w - h) `quot` 2) 0 h h)
--                                            rgb
--                            | otherwise =
--                      resizeSquare $ 'crop' ('Rect' 0 ((h - w) `quot` 2) w w)
--                                            rgb
--
--              'save' \"miniature.png\" miniature
--   where
--      resizeSquare :: 'RGBDelayed' -> 'RGB'
--      resizeSquare !img = 'resize' 'Bilinear' ('ix2' 150 150) img
-- @
--
-- Notice as 'crop' returns a 'Delayed' image and thus doesn't allocate an
-- intermediate image.
module Vision.Image (
      module Vision.Image.Grey
    , module Vision.Image.Filter
    , module Vision.Image.HSV
    , module Vision.Image.Interpolate
    , module Vision.Image.Mutable
    , module Vision.Image.RGB
    , module Vision.Image.RGBA
    , module Vision.Image.Storage
    , module Vision.Image.Threshold
    , module Vision.Image.Transform
    , module Vision.Image.Type
    ) where

import Vision.Image.Grey
import Vision.Image.Filter
import Vision.Image.HSV
import Vision.Image.Interpolate
import Vision.Image.Mutable
import Vision.Image.RGB
import Vision.Image.RGBA
import Vision.Image.Storage
import Vision.Image.Threshold
import Vision.Image.Transform
import Vision.Image.Type
