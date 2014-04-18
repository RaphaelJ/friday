-- | Images are manipulated by their 'Image' type-class instance.
--
-- The 'Manifest' representation uses an internal 'Vector' to represent the
-- image whereas the 'Delayed' representation uses a function to generate
-- pixels. Most transformation functions are generic to both representations in
-- the way they apply to any type which implements the 'Image' type-class.
--
-- The 'Delayed' image should be used as intermediate representations of
-- complex image transformations.
--
-- The following example reads an image from a file, applies a composition of
-- transformations by creating a centred and squared miniature and then writes
-- the result to a file:
--
-- @
-- main = do
--         img <- 'load' \"image.png\"
--         let 'Size' w h  = 'getSize' img
--             miniature
--                 | w > h =
--                     resizeSquare $ 'crop' img (Rect ((w - h) `quot` 2) 0 h h)
--                 | otherwise =
--                     resizeSquare $ 'crop' img (Rect 0 ((h - w) `quot` 2) w w)
--         save \"miniature.png\" miniature
--       where
--         resizeSquare :: 'RGBDelayed' -> 'RGBImage'
--         resizeSquare !img = 'resize' img 'Bilinear' ('Size' 150 150)
-- @
--
-- Notice as 'crop' returns a 'Delayed' image and thus doesn't allocate an
-- intermediate image.
module Vision.Image (
      module Vision.Image.GreyImage
    , module Vision.Image.HSVImage
    , module Vision.Image.Interpolate
    , module Vision.Image.RGBImage
    , module Vision.Image.RGBAImage
    , module Vision.Image.Storage
    , module Vision.Image.Transform
    , module Vision.Image.Type
    ) where

import Vision.Image.GreyImage
import Vision.Image.HSVImage
import Vision.Image.Interpolate
import Vision.Image.RGBImage
import Vision.Image.RGBAImage
import Vision.Image.Storage
import Vision.Image.Transform
import Vision.Image.Type
