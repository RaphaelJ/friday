{-# LANGUAGE BangPatterns, FlexibleContexts #-}
import Criterion.Main
import Data.Int
import Data.Word

import Vision.Image (
      GreyImage, HSVImage, RGBAImage, RGBImage, RGBDelayed, InterpolMethod
    )
import qualified Vision.Image as I
import Vision.Histogram (Histogram)
import qualified Vision.Histogram as H
import Vision.Primitive

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- I.load path Nothing
    let !(Z :. h :. w) = I.shape rgb
        !rgb           = I.convert io             :: RGBImage
        !rgba          = I.convert rgb            :: RGBAImage
        !grey          = I.convert rgb            :: GreyImage
        !hsv           = I.convert rgb            :: HSVImage
        !hist          = H.histogram grey Nothing :: H.Histogram DIM1 Int32
        !hist2D        = H.histogram2D grey (ix3 256 3 3)
                                                  :: H.Histogram DIM3 Int32

    defaultMain [
          bgroup "IO" [
              bench "load" $ whnfIO $ I.load path Nothing
            ]
        , bgroup "conversion" [
              bench "RGB to grey" $
                whnf (I.convert :: RGBImage  -> GreyImage) rgb
            , bench "RGBA to grey" $
                whnf (I.convert :: RGBAImage -> GreyImage) rgba
            , bench "RGBA to RGB" $
                whnf (I.convert :: RGBAImage -> RGBImage)  rgba
            , bench "RGB to RGBA" $
                whnf (I.convert :: RGBImage  -> RGBAImage) rgb
            , bench "RGB to HSV" $
                whnf (I.convert :: RGBImage  -> HSVImage)  rgb
            , bench "HSV to RGB" $
                whnf (I.convert :: HSVImage  -> RGBImage)  hsv
            ]
        , bgroup "crop" [
              bench "RGB" $
                whnf (I.crop rgb :: Rect -> RGBImage)
                     (Rect (w `quot` 2) (h `quot` 2) (w `quot` 2) (h `quot` 2))
            ]
        , bgroup "resize" [
              bench "truncate-integer 50%" $
                whnf (resize' rgb I.TruncateInteger)
                     (Z :. (h `quot` 2) :. (w `quot` 2))
            , bench "truncate-integer 200%" $
                whnf (resize' rgb I.TruncateInteger) (Z :. (h * 2) :. (w * 2))
            , bench "nearest-neighbor 50%" $
                whnf (resize' rgb I.NearestNeighbor)
                     (Z :. (h `quot` 2) :. (w `quot` 2))
            , bench "nearest-neighbor 200%" $
                whnf (resize' rgb I.NearestNeighbor) (Z :. (h * 2) :. (w * 2))
            , bench "bilinear 50%" $
                whnf (resize' rgb I.Bilinear)
                     (Z :. (h `quot` 2) :. (w `quot` 2))
            , bench "bilinear 200%" $
                whnf (resize' rgb I.Bilinear) (Z :. (h * 2) :. (w * 2))
            ]
        , bgroup "filter" [
              bench "erode"         $ whnf erode' grey
            , bench "blur"          $ whnf blur' grey
            , bench "gaussian blur" $ whnf gaussianBlur' grey
            , bench "scharr"        $ whnf scharr' grey
            , bench "sobel"         $ whnf sobel' grey
            ]
        , bgroup "flip" [
              bench "horizontal" $
                whnf (I.horizontalFlip :: RGBImage -> RGBImage) rgb
            , bench "vertical"   $
                whnf (I.verticalFlip :: RGBImage -> RGBImage)   rgb
            ]
        , bgroup "histogram" [
              bench "calculate 1D histogram of a grey image" $
                whnf (\img -> H.histogram img Nothing :: Histogram DIM1 Int32)
                     grey
            , bench "calculate 3D histogram of a RGB image" $
                whnf (\img -> H.histogram img Nothing :: Histogram DIM3 Int32)
                     rgb
            , bench "calculate 3D histogram (9 regions) of a grey image" $
                whnf (\img -> H.histogram2D img (Z :. 256 :. 3 :. 3)
                              :: Histogram DIM3 Int32)
                     grey

            , bench "reduce an Int32 histogram" $ whnf H.reduce hist2D
            , bench "resize an Int32 histogram" $ whnf (H.resize hist)
                                                       (Z :. 128)

            , bench "cumulative Int32 histogram" $ whnf H.cumulative hist

            , bench "normalize histogram" $
                whnf (\hist' -> H.normalize hist' 1 :: Histogram DIM1 Double)
                     hist
            , bench "equalize grey image" $
                whnf (H.equalizeImage :: GreyImage -> GreyImage) grey

            , bench "correlation comparison" $
                whnf (H.compareCorrel hist :: Histogram DIM1 Int32 -> Double)
                     hist
            , bench "chi-square comparison" $
                whnf (H.compareChi hist :: Histogram DIM1 Int32 -> Double) hist
            , bench "intersection comparison" $
                whnf (H.compareIntersect hist :: Histogram DIM1 Int32 -> Int32)
                     hist
            , bench "EMD comparison" $ whnf (H.compareEMD hist) hist

            , bench "2D correlation comparison" $
                whnf (H.compareCorrel hist2D :: Histogram DIM3 Int32 -> Double)
                     hist2D
            , bench "2D chi-square comparison 2D" $
                whnf (H.compareChi hist2D :: Histogram DIM3 Int32 -> Double)
                     hist2D
            , bench "2D intersection comparison 2D" $
                whnf (H.compareIntersect hist2D
                                            :: Histogram DIM3 Int32 -> Int32)
                     hist2D
            ]
        , bgroup "application" [
              bench "miniature 150x150" $ whnf miniature rgb
            ]
        ]
  where
    resize' :: RGBImage -> InterpolMethod -> Size -> RGBImage
    resize' = I.resize
    {-# INLINE resize' #-}

    erode' :: GreyImage -> GreyImage
    erode' img = img `I.apply` I.erode 1

    blur' :: GreyImage -> GreyImage
    blur' img =
        let filt = I.blur 1 :: I.SeparableFilter I.GreyPixel Word32 I.GreyPixel
        in img `I.apply` filt

    gaussianBlur' :: GreyImage -> GreyImage
    gaussianBlur' img =
        let filt = I.gaussianBlur 1 Nothing :: I.SeparableFilter I.GreyPixel
                                                                 Float
                                                                 I.GreyPixel
        in img `I.apply` filt

    sobel' :: GreyImage -> I.Manifest Int16
    sobel' img = img `I.apply` I.sobel 1 I.DerivativeX

    scharr' :: GreyImage -> I.Manifest Int16
    scharr' img = img `I.apply` I.scharr I.DerivativeX

    miniature !rgb =
        let Z :. h :. w = I.shape rgb
        in if w > h
              then resizeSquare $ I.crop rgb (Rect ((w - h) `quot` 2) 0 h h)
              else resizeSquare $ I.crop rgb (Rect 0 ((h - w) `quot` 2) w w)

    resizeSquare :: RGBDelayed -> RGBImage
    resizeSquare !rgb = I.resize rgb I.Bilinear (Z :. 150 :. 150)
