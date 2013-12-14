{-# LANGUAGE BangPatterns, FlexibleContexts #-}
import Criterion.Main

import Vision.Image

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- load path
    let !rgb        = convert io  :: RGBImage
        !rgba       = convert rgb :: RGBAImage
        !(Size w h) = getSize rgb

    defaultMain [
          bgroup "IO" [
              bench "load" $ whnfIO $ load path
            ]
        , bgroup "conversion" [
              bench "RGB to grey" $
                whnf (convert :: RGBImage -> GreyImage) rgb
            , bench "RGBA to grey" $
                whnf (convert :: RGBAImage -> GreyImage) rgba
            , bench "RGB to RGBA" $
                whnf (convert :: RGBImage -> RGBAImage) rgb
            , bench "RGBA to RGB" $
                whnf (convert :: RGBAImage -> RGBImage) rgba
            ]
        , bgroup "crop" [
              bench "RGB" $
                whnf (crop rgb :: Rect -> RGBImage)
                     (Rect (w `quot` 2) (h `quot` 2) (w `quot` 2) (h `quot` 2))
            ]
        , bgroup "resize" [
              bench "truncate-integer 50%" $
                whnf (resize' rgb TruncateInteger)
                     (Size (w `quot` 2) (h `quot` 2))
            , bench "truncate-integer 200%" $
                whnf (resize' rgb TruncateInteger) (Size (w * 2) (h * 2))
            , bench "nearest-neighbor 50%" $
                whnf (resize' rgb NearestNeighbor)
                     (Size (w `quot` 2) (h `quot` 2))
            , bench "nearest-neighbor 200%" $
                whnf (resize' rgb NearestNeighbor) (Size (w * 2) (h * 2))
            , bench "bilinear 50%" $
                whnf (resize' rgb Bilinear)
                     (Size (w `quot` 2) (h `quot` 2))
            , bench "bilinear 200%" $
                whnf (resize' rgb Bilinear) (Size (w * 2) (h * 2))
            ]
        , bgroup "flip" [
              bench "horizontal" $ whnf (horizontalFlip :: RGBImage -> RGBImage)
                                        rgb
            , bench "vertical"   $ whnf (verticalFlip :: RGBImage -> RGBImage)
                                        rgb
            ]
        , bgroup "application" [
              bench "miniature 150x150" $ whnf miniature rgb
            ]
        ]
  where
    resize' :: RGBImage -> InterpolMethod -> Size -> RGBImage
    resize' = resize
    {-# INLINE resize' #-}

    miniature !rgb =
        let Size w h = getSize rgb
        in if w > h then resizeSquare $ crop rgb (Rect ((w - h) `quot` 2) 0 h h)
                    else resizeSquare $ crop rgb (Rect 0 ((h - w) `quot` 2) w w)

    resizeSquare :: RGBDelayed -> RGBImage
    resizeSquare !rgb = resize rgb Bilinear (Size 150 150)
