{-# LANGUAGE BangPatterns #-}
import Criterion.Main

import Vision.Image

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- load path
    let !img        = convert io :: RGBImage F
        Z :. h :. w = extent img

    defaultMain [
          bgroup "IO" [
              bench "load" $ whnfIO $ load path
            ]
        , bgroup "conversion" [
              bench "RGB to grey" $
                whnf (  (computeS :: GreyImage D -> GreyImage F)
                      . (convert  :: RGBImage  F -> GreyImage D))
                     img
            , bench "RGB to RGBA" $
                whnf (  (computeS :: RGBAImage D -> RGBAImage F)
                      . (convert  :: RGBImage  F -> RGBAImage D))
                     img
            ]
        , bgroup "crop" [
              bench "RGB" $
                whnf (crop img) (Rect (w `quot` 2) (h `quot` 2)
                                      (w `quot` 2) (h `quot` 2))
            ]
        , bgroup "resize" [
              bench "truncate-integer 50%" $
                whnf (resize img TruncateInteger)
                    (Z :. (h `quot` 2) :. (w `quot` 2))
            , bench "truncate-integer 200%" $
                whnf (resize img TruncateInteger) (Z :. (h * 2) :. (w * 2))
            , bench "nearest-neighbor 50%" $
                whnf (resize img NearestNeighbor)
                    (Z :. (h `quot` 2) :. (w `quot` 2))
            , bench "nearest-neighbor 200%" $
                whnf (resize img NearestNeighbor) (Z :. (h * 2) :. (w * 2))
            , bench "bilinear 50%" $
                whnf (resize img Bilinear)
                    (Z :. (h `quot` 2) :. (w `quot` 2))
            , bench "bilinear 200%" $
                whnf (resize img Bilinear) (Z :. (h * 2) :. (w * 2))
            ]
        , bgroup "flip" [
              bench "horizontal" $ whnf horizontalFlip img
            , bench "vertical"   $ whnf verticalFlip   img
            ]
        ]