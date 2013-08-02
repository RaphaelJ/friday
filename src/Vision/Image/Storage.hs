{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
-- | Uses the repa-devil package to load and save images.
module Vision.Image.Storage (IOImage (..), load, save) where

import Control.Applicative ((<$>))
import Data.Array.Repa (D, (:.) (..), computeP, extent, reshape)
import Data.Array.Repa.Repr.ForeignPtr (F)
import qualified Data.Array.Repa.IO.DevIL as IL
import Data.Convertible (Convertible (..), convert)

import Vision.Image.GreyImage.Type (GreyImage (..))
import Vision.Image.RGBAImage.Type (RGBAImage (..))
import Vision.Image.RGBImage.Type (RGBImage (..))

data IOImage = GreyIOImage (GreyImage D)
             | RGBAIOImage (RGBAImage F) | RGBIOImage  (RGBImage  F)

instance Convertible IOImage IOImage where
    safeConvert = Right

load :: FilePath -> IO (Either String IOImage)
load path = do
    ilImg <- IL.runIL $ IL.readImage path
    return $ case ilImg of
        IL.Grey img -> let sh = extent img :. 1
                       in Right $ GreyIOImage $ GreyImage $ reshape sh img
        IL.RGBA img -> Right $ RGBAIOImage $ RGBAImage img
        IL.RGB  img -> Right $ RGBIOImage  $ RGBImage  img
        _           -> Left "Unsupported format"

save :: (Convertible i IOImage) => FilePath -> i -> IO ()
save path img = do
    ilImg <- case convert img of
                GreyIOImage (GreyImage arr) ->
                    let sh :. ~1 = extent arr -- FIXME: avoid to recompute.
                    in IL.Grey <$> computeP (reshape sh arr)
                RGBAIOImage (RGBAImage arr) -> return $ IL.RGBA arr
                RGBIOImage  (RGBImage  arr) -> return $ IL.RGB  arr
    IL.runIL $ IL.writeImage path ilImg
