{-# LANGUAGE BangPatterns, FlexibleContexts #-}
import Control.Monad.ST.Safe (ST)
import Criterion.Main
import Data.Int

import Vision.Image (
      Grey, HSV, RGBA, RGB, RGBDelayed, InterpolMethod
    )
import qualified Vision.Detector.Edge as D (canny)
import qualified Vision.Image as I
import Vision.Histogram (Histogram)
import qualified Vision.Histogram as H
import Vision.Primitive

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- I.load Nothing path
    let !(Z :. h :. w) = I.shape rgb
        !halfSize      = Rect (w `quot` 2) (h `quot` 2)
                              (w `quot` 2) (h `quot` 2)
        !rgb           = I.convert io             :: RGB
        !rgba          = I.convert rgb            :: RGBA
        !grey          = I.convert rgb            :: Grey
        !edges         = canny' grey
        !hsv           = I.convert rgb            :: HSV
        !hist          = H.histogram Nothing grey :: H.Histogram DIM1 Int32
        !hist2D        = H.histogram2D (ix3 256 3 3) grey
                                                  :: H.Histogram DIM3 Int32

    defaultMain [
          bgroup "IO" [
              bench "load" $ whnfIO $ I.load Nothing path
            ]
        , bgroup "conversion" [
              bench "RGB to grey"  $ whnf (I.convert :: RGB  -> Grey) rgb
            , bench "RGBA to grey" $ whnf (I.convert :: RGBA -> Grey) rgba
            , bench "RGBA to RGB"  $ whnf (I.convert :: RGBA -> RGB)  rgba
            , bench "RGB to RGBA"  $ whnf (I.convert :: RGB  -> RGBA) rgb
            , bench "RGB to HSV"   $ whnf (I.convert :: RGB  -> HSV)  rgb
            , bench "HSV to RGB"   $ whnf (I.convert :: HSV  -> RGB)  hsv
            ]
        , bgroup "crop" [
              bench "RGB" $ whnf (I.crop halfSize :: RGB -> RGB) rgb
            ]
        , bgroup "detector" [
              bench "Canny's edge detector" $ whnf canny' grey
            ]
        , bgroup "filter" [
              bench "erode"         $ whnf erode' grey
            , bench "blur"          $ whnf blur' grey
            , bench "gaussian blur" $ whnf gaussianBlur' grey
            , bench "scharr"        $ whnf scharr' grey
            , bench "sobel"         $ whnf sobel' grey
            , bench "mean"          $ whnf mean' grey
            ]
        , bgroup "flip" [
              bench "horizontal" $ whnf (I.horizontalFlip :: RGB -> RGB) rgb
            , bench "vertical"   $ whnf (I.verticalFlip   :: RGB -> RGB) rgb
            ]
        , bench "flood-fill" $ whnf floodFill' edges
        , bgroup "histogram" [
              bench "calculate 1D histogram of a grey image" $
                whnf (H.histogram Nothing :: Grey -> Histogram DIM1 Int32) grey
            , bench "calculate 3D histogram of a RGB image" $
                whnf (H.histogram Nothing :: RGB  -> Histogram DIM3 Int32) rgb
            , bench "calculate 3D histogram (9 regions) of a grey image" $
                whnf (H.histogram2D (ix3 256 3 3)
                                    :: Grey -> Histogram DIM3 Int32)
                     grey

            , bench "reduce an Int32 histogram" $ whnf H.reduce hist2D
            , bench "resize an Int32 histogram" $ whnf (H.resize (ix1 128))
                                                       hist

            , bench "cumulative Int32 histogram" $ whnf H.cumulative hist

            , bench "normalize histogram" $
                whnf (H.normalize 1
                        :: Histogram DIM1 Int32 -> Histogram DIM1 Double)
                     hist
            , bench "equalize grey image" $
                whnf (H.equalizeImage :: Grey -> Grey) grey

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
        , bgroup "resize" [
              bench "truncate-integer 50%" $
                whnf (resize' I.TruncateInteger (ix2 (h `quot` 2) (w `quot` 2)))
                     rgb
            , bench "truncate-integer 200%" $
                whnf (resize' I.TruncateInteger (ix2 (h * 2) (w * 2))) rgb
            , bench "nearest-neighbor 50%" $
                whnf (resize' I.NearestNeighbor (ix2 (h `quot` 2) (w `quot` 2)))
                     rgb
            , bench "nearest-neighbor 200%" $
                whnf (resize' I.NearestNeighbor (ix2 (h * 2) (w * 2))) rgb
            , bench "bilinear 50%" $
                whnf (resize' I.Bilinear (ix2 (h `quot` 2) (w `quot` 2))) rgb
            , bench "bilinear 200%" $
                whnf (resize' I.Bilinear (ix2 (h * 2) (w * 2))) rgb
            ]
        , bgroup "threshold" [
              bench "simple threshold"   $ whnf threshold'         grey
            , bench "adaptive threshold" $ whnf adaptiveThreshold' grey
            , bench "Otsu's method"      $ whnf otsu'              grey
            , bench "SCW"                $ whnf scw'               grey
            ]


        , bgroup "application" [
              bench "miniature 150x150" $ whnf miniature rgb
            ]
        ]
  where
    canny' :: Grey -> Grey
    canny' !img = D.canny 2 256 1024 img

    erode' :: Grey -> Grey
    erode' !img = I.erode 1 `I.apply` img

    blur'  :: Grey -> Grey
    blur' !img =
        let filt = I.blur 1 :: I.Blur I.GreyPixel Int16 I.GreyPixel
        in filt `I.apply` img

    gaussianBlur' :: Grey -> Grey
    gaussianBlur' !img =
        let filt = I.gaussianBlur 1 Nothing :: I.Blur I.GreyPixel Float
                                                      I.GreyPixel
        in filt `I.apply` img

    sobel' :: Grey -> I.Manifest Int16
    sobel' !img = I.sobel 1 I.DerivativeX `I.apply` img

    scharr' :: Grey -> I.Manifest Int16
    scharr' !img = I.scharr I.DerivativeX `I.apply` img

    mean' :: Grey -> I.Manifest Float
    mean' !img =
        let filt = I.mean (ix2 3 3) :: I.Mean I.GreyPixel Int16 Float
        in filt `I.apply` img

    floodFill' :: Grey -> I.Grey
    floodFill' img =
        I.create $ do
            mut <- I.thaw img :: ST s (I.MutableManifest I.GreyPixel s)
            I.floodFill (ix2 5 5) 255 mut
            return mut

    resize' :: InterpolMethod -> Size -> RGB -> RGB
    resize' = I.resize

    threshold' :: Grey -> Grey
    threshold' !img = I.threshold (> 127) (I.BinaryThreshold 0 255) img

    adaptiveThreshold' :: Grey -> Grey
    adaptiveThreshold' !img =
        let filt :: I.AdaptiveThreshold I.GreyPixel Float I.GreyPixel
            filt = I.adaptiveThreshold (I.GaussianKernel Nothing) 1 0
                                       (I.BinaryThreshold 0 255)
        in filt `I.apply` img

    otsu' :: Grey -> Grey
    otsu' !img = I.otsu (I.BinaryThreshold 0 255) img

    scw' :: Grey -> Grey
    scw' !img = I.scw (ix2 5 5) (ix2 15 15) (5 :: Double)
                      (I.BinaryThreshold 255 0) img

    miniature !rgb =
        let Z :. h :. w = I.shape rgb
        in if w > h
              then resizeSquare $ I.crop (Rect ((w - h) `quot` 2) 0 h h) rgb
              else resizeSquare $ I.crop (Rect 0 ((h - w) `quot` 2) w w) rgb

    resizeSquare :: RGBDelayed -> RGB
    resizeSquare = I.resize I.Bilinear (Z :. 150 :. 150)
