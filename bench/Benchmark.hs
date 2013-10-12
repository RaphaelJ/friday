{-# LANGUAGE BangPatterns, FlexibleContexts #-}
import Criterion.Main

import Vision.Image

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- load path
    let !img        = convert io :: RGBImage
        !(Size w h) = getSize img

    defaultMain [
          bgroup "IO" [
              bench "load" $ whnfIO $ load path
            ]
        , bgroup "conversion" [
              bench "RGB to grey" $
                whnf (convert :: RGBImage -> GreyImage) img
            , bench "RGB to RGBA" $
                whnf (convert :: RGBImage -> RGBAImage) img
            ]
        , bgroup "crop" [
              bench "RGB" $
                whnf (crop img :: Rect -> RGBImage)
                     (Rect (w `quot` 2) (h `quot` 2) (w `quot` 2) (h `quot` 2))
            ]
        , bgroup "resize" [
              bench "truncate-integer 50%" $
                whnf (resize' img TruncateInteger)
                     (Size (w `quot` 2) (h `quot` 2))
            , bench "truncate-integer 200%" $
                whnf (resize' img TruncateInteger) (Size (w * 2) (h * 2))
            , bench "nearest-neighbor 50%" $
                whnf (resize' img NearestNeighbor)
                     (Size (w `quot` 2) (h `quot` 2))
            , bench "nearest-neighbor 200%" $
                whnf (resize' img NearestNeighbor) (Size (w * 2) (h * 2))
            , bench "bilinear 50%" $
                whnf (resize' img Bilinear)
                     (Size (w `quot` 2) (h `quot` 2))
            , bench "bilinear 200%" $
                whnf (resize' img Bilinear) (Size (w * 2) (h * 2))
            ]
        , bgroup "flip" [
              bench "horizontal" $ whnf (horizontalFlip :: RGBImage -> RGBImage)
                                        img
            , bench "vertical"   $ whnf (verticalFlip :: RGBImage -> RGBImage)
                                        img
            ]
        , bgroup "application" [
              bench "miniature 150x150" $ whnf miniature img
            ]
        ]
  where
    resize' :: RGBImage -> InterpolMethod -> Size -> RGBImage
    resize' = resize
    {-# INLINE resize' #-}

    miniature !img =
        let Size w h = getSize img
        in if w > h then resizeSquare $ crop img (Rect ((w - h) `quot` 2) 0 h h)
                    else resizeSquare $ crop img (Rect 0 ((h - w) `quot` 2) w w)

    resizeSquare :: RGBDelayed -> RGBImage
    resizeSquare !img = resize img Bilinear (Size 150 150)
