{-# LANGUAGE BangPatterns, GADTs #-}

-- | Provides high level functions to apply filters on images.
module Vision.Image.Filter (
      Filter (..), Kernel (..), FilterFold (..), apply
    ) where

import Vision.Image.Interpolate (Interpolable, bilinearInterpol)
import Vision.Image.Type (MaskedImage (..), Image (..), FromFunction (..))
import Vision.Primitive (Z (..), (:.) (..), RPoint (..), Rect (..), Size, DIM1, DIM2, ix2)

data Filter input acc output = Filter {
      fKernelSize   :: !DIM2
    , fKernelCenter :: !KernelAnchor
    , fKernel       :: !(Kernel     input acc)
    , fFold         :: !(FilterFold input acc)
    , fPost         :: !(acc -> output)
    }

data KernelAnchor = KernelAnchor !DIM2
                  | KernelAnchorCenter

data Kernel input acc = Kernel !(DIM2 -> input -> acc -> acc)
                      | SeparableKernel !(DIM1 -> input -> acc -> acc)
                                        !(DIM1 -> acc   -> acc -> acc)

data FilterFold input acc where
    FilterFold  :: acc -> FilterFold input acc
    FilterFold1 ::        FilterFold input input

apply :: (Image src, FromFunction dst)
      => src
      -> Filter (ImagePixel src) acc (FromFunctionPixel dst)
      -> dst
apply = undefined
-- apply img (Filter size center (Kernel f) fold post) =
--     fromFunction (shape img) $ \(Z :. y :. x) ->
--         post 
-- 
--   where
--     center' | 

erode :: Ord input => Filter input input input
erode = Filter (ix2 3 3) KernelAnchorCenter
               (SeparableKernel (const min) (const min)) FilterFold1 id

blur :: Integral a => Int -> Filter a Int a
blur radius =
    Filter (ix2 size size) KernelAnchorCenter (SeparableKernel one one)
           (FilterFold 0) (\acc -> fromIntegral $ acc `div` size `div` size)
  where
    !size = radius * 2 + 1

    one _ !v !acc = acc + fromIntegral v