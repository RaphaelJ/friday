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
          bench "load" $ whnfIO $ load path

        , bench "truncate-integer resize 50%" $
            whnf (resize TruncateInteger img)
                 (Z :. (h `quot` 2) :. (w `quot` 2))
        , bench "truncate-integer resize 200%" $
            whnf (resize TruncateInteger img) (Z :. (h * 2) :. (w * 2))
        , bench "nearest-neighbor resize 50%" $
            whnf (resize NearestNeighbor img)
                 (Z :. (h `quot` 2) :. (w `quot` 2))
        , bench "nearest-neighbor resize 200%" $
            whnf (resize NearestNeighbor img) (Z :. (h * 2) :. (w * 2))
        , bench "bilinear resize 50%" $
            whnf (resize Bilinear img)
                 (Z :. (h `quot` 2) :. (w `quot` 2))
        , bench "bilinear resize 200%" $
            whnf (resize Bilinear img) (Z :. (h * 2) :. (w * 2))

        , bench "RGB to grey" $
            whnf (  (computeS :: GreyImage D -> GreyImage F)
                  . (convert  :: RGBImage  F -> GreyImage D))
                 img
        ]
