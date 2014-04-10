{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , ParallelListComp, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains functions to compute and manipulate histograms as well as some
-- images transformations which are histograms-based.
-- 
-- Every polymorphic function is specialised for histograms of 'Int32', 'Double'
-- and 'Float'. Other types could be specialized as every polymorphic functions
-- are declared @INLINABLE@.
module Vision.Histogram (
    -- * Types & helpers
      Histogram (..), HistogramShape (..), ToHistogram (..)
    , index, linearIndex, map, assocs
    -- * Histogram computations
    , histogram,  histogram2D, reduce, resize, cumulative, normalize
    -- * Images processing
    , equalizeImage
    -- * Histogram comparisons
    , compareCorrel, compareChi, compareIntersect, compareEMD
    ) where

import Data.Int
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Data.Vector.Unboxed (Unbox)
import Foreign.Storable (Storable)
import Prelude hiding (map)

import Vision.Image (
      Image, Pixel, ImagePixel, FromFunction
    , GreyImage, GreyPixel (..), HSVImage, HSVPixel (..)
    , RGBAImage, RGBAPixel (..), RGBImage, RGBPixel (..)
    )
import qualified Vision.Image as I
import Vision.Primitive (
      Z (..), (:.) (..), Shape (..), DIM1, DIM3, DIM4, DIM5, DIM6
    , ix1, ix3, ix4
    )

-- There is no rule to simplify the conversion from Int32 to Double and Float
-- when using realToFrac. Both conversions are using a temporary yet useless
-- Rational value.

{-# RULES
"realToFrac/Int32->Double" realToFrac = fromIntegral :: Int32 -> Double
"realToFrac/Int32->Float"  realToFrac = fromIntegral :: Int32 -> Float
  #-}

-- Types -----------------------------------------------------------------------

data Histogram sh a = Histogram {
      shape  :: !sh
    , vector :: !(Vector a) -- ^ Values of the histogram in row-major order.
    } deriving (Eq, Ord, Show)

-- | Subclass of 'Shape' which defines how to resize a shape so it will fit
-- inside a resized histogram.
class Shape sh => HistogramShape sh where
    -- | Given a number of bins, reduces an index value so it will be mapped to
    -- a bin.
    toBin :: sh -- ^ The number of bins we are mapping to.
          -> sh -- ^ The number of possible values of the original index.
          -> sh -- ^ The original index.
          -> sh -- ^ The index of the bin in the histogram.

instance HistogramShape Z where
    toBin _ _ _ = Z
    {-# INLINE toBin #-}

instance HistogramShape sh => HistogramShape (sh :. Int) where
    toBin !(shBins :. bins) !(shMaxBins :. maxBins) !(shIx :. ix)
        | bins == maxBins = inner :. ix
        | otherwise       = inner :. (ix * bins `quot` maxBins)
      where
        inner = toBin shBins shMaxBins shIx
    {-# INLINE toBin #-}

-- | This class defines how many dimensions a histogram will have and what will
-- be the default number of bins.
class (Pixel p, Shape (PixelValueSpace p)) => ToHistogram p where
    -- | Gives the value space of a pixel. Single-channel pixels will be 'DIM1'
    -- whereas three-channels pixels will be 'DIM3'.
    -- This is used to determine the rank of the generated histogram.
    type PixelValueSpace p

    pixToIndex :: p -> PixelValueSpace p

    -- | Returns the maximum number of different values an index can take for
    -- each dimension of the histogram (aka. the maximum index returned by
    -- 'pixToIndex' minus one).
    domainSize :: p -> PixelValueSpace p

instance ToHistogram GreyPixel where
    type PixelValueSpace GreyPixel = DIM1

    pixToIndex !(GreyPixel val) = ix1 $ int val
    {-# INLINE pixToIndex #-}

    domainSize _ = ix1 256

instance ToHistogram RGBAPixel where
    type PixelValueSpace RGBAPixel = DIM4

    pixToIndex !(RGBAPixel r g b a) = ix4 (int r) (int g) (int b) (int a)
    {-# INLINE pixToIndex #-}

    domainSize _ = ix4 256 256 256 256

instance ToHistogram RGBPixel where
    type PixelValueSpace RGBPixel = DIM3

    pixToIndex !(RGBPixel r g b) = ix3 (int r) (int g) (int b)
    {-# INLINE pixToIndex #-}

    domainSize _ = ix3 256 256 256

instance ToHistogram HSVPixel where
    type PixelValueSpace HSVPixel = DIM3

    pixToIndex !(HSVPixel h s v) = ix3 (int h) (int s) (int v)
    {-# INLINE pixToIndex #-}

    domainSize _ = ix3 180 256 256

-- Functions -------------------------------------------------------------------

index :: (Shape sh, Storable a) => Histogram sh a -> sh -> a
index !hist = linearIndex hist . toLinearIndex (shape hist)
{-# INLINE index #-}

-- | Returns the value at the index as if the histogram was a single dimension
-- vector (row-major representation).
linearIndex :: (Shape sh, Storable a) => Histogram sh a -> Int -> a
linearIndex !hist = (!) (vector hist)
{-# INLINE linearIndex #-}

map :: (Storable a, Storable b) => (a -> b) -> Histogram sh a -> Histogram sh b
map f !(Histogram sh vec) = Histogram sh (V.map f vec)
{-# INLINE map #-}

-- | Returns all index/value pairs from the histogram.
assocs :: (Shape sh, Storable a) => Histogram sh a -> [(sh, a)]
assocs !(Histogram sh vec) = [ (ix, v) | ix <- shapeList sh
                                       | v <- V.toList vec ]
{-# INLINE assocs #-}

-- | Computes an histogram from a (possibly) multi-channel image.
-- If the size of the histogram is not given, there will be as many bins as the
-- range of values of pixels of the original image (see 'domainSize').
-- If the size of the histogram is specified, every bin of a given dimension
-- will be of the same size (uniform histogram).
histogram :: (Image i, ToHistogram (ImagePixel i), Storable a, Num a
            , HistogramShape (PixelValueSpace (ImagePixel i)))
         => i -> Maybe (PixelValueSpace (ImagePixel i))
         -> Histogram (PixelValueSpace (ImagePixel i)) a
histogram img mSize =
    let initial = V.replicate nBins 0
        ones    = V.replicate nPixs 1
        ixs     = V.map toIndex (I.vector img)
    in Histogram size (V.accumulate_ (+) initial ixs ones)
  where
    !size = case mSize of Just s  -> s
                          Nothing -> maxSize
    !maxSize = domainSize (I.pixel img)
    !nChans = I.nChannels img
    !nPixs = shapeLength (I.shape img) * nChans
    !nBins = shapeLength size

    toIndex !p = toLinearIndex size $!
        case mSize of Just _  -> toBin size maxSize $! pixToIndex p
                      Nothing -> pixToIndex p
    {-# INLINE toIndex #-}
{-# SPECIALIZE histogram :: GreyImage -> Maybe DIM1 -> Histogram DIM1 Int32
                         ,  GreyImage -> Maybe DIM1 -> Histogram DIM1 Double
                         ,  GreyImage -> Maybe DIM1 -> Histogram DIM1 Float
                         ,  HSVImage  -> Maybe DIM3 -> Histogram DIM3 Int32
                         ,  HSVImage  -> Maybe DIM3 -> Histogram DIM3 Double
                         ,  HSVImage  -> Maybe DIM3 -> Histogram DIM3 Float
                         ,  RGBAImage -> Maybe DIM4 -> Histogram DIM4 Int32
                         ,  RGBAImage -> Maybe DIM4 -> Histogram DIM4 Double
                         ,  RGBAImage -> Maybe DIM4 -> Histogram DIM4 Float
                         ,  RGBImage  -> Maybe DIM3 -> Histogram DIM3 Int32
                         ,  RGBImage  -> Maybe DIM3 -> Histogram DIM3 Double
                         ,  RGBImage  -> Maybe DIM3 -> Histogram DIM3 Float  #-}
{-# INLINABLE histogram #-}

-- | Similar to 'histogram' but adds two dimensions for the y and x-coordinates
-- of the sampled points. This way, the histogram will map different regions of
-- the original image. For example, an RGB image will be mapped as
-- @Z :. red channel :. green channel :. blue channel :. y region :. x region@.
-- As there is no reason to create an histogram as large as the number of pixels
-- of the image, a size is always needed.
histogram2D :: (Image i, ToHistogram (ImagePixel i), Storable a, Num a
            , HistogramShape (PixelValueSpace (ImagePixel i)))
            => i -> (PixelValueSpace (ImagePixel i)) :. Int :. Int
            -> Histogram ((PixelValueSpace (ImagePixel i)) :. Int :. Int) a
histogram2D img size =
    let initial = V.replicate nBins 0
        ones    = V.replicate nPixs 1
        imgIxs  = V.iterateN nPixs (shapeSucc imgSize) shapeZero
        ixs     = V.zipWith toIndex imgIxs (I.vector img)
    in Histogram size (V.accumulate_ (+) initial ixs ones)
  where
    !imgSize@(Z :. h :. w) = I.shape img
    !maxSize = domainSize (I.pixel img) :. h :. w
    !nChans = I.nChannels img
    !nPixs = shapeLength (I.shape img) * nChans
    !nBins = shapeLength size

    toIndex !(Z :. y :. x) !p =
        let !ix = (pixToIndex p) :. y :. x
        in toLinearIndex size $! toBin size maxSize ix
    {-# INLINE toIndex #-}
{-# SPECIALIZE histogram2D :: GreyImage -> DIM3 -> Histogram DIM3 Int32
                           ,  GreyImage -> DIM3 -> Histogram DIM3 Double
                           ,  GreyImage -> DIM3 -> Histogram DIM3 Float
                           ,  HSVImage  -> DIM5 -> Histogram DIM5 Int32
                           ,  HSVImage  -> DIM5 -> Histogram DIM5 Double
                           ,  HSVImage  -> DIM5 -> Histogram DIM5 Float
                           ,  RGBAImage -> DIM6 -> Histogram DIM6 Int32
                           ,  RGBAImage -> DIM6 -> Histogram DIM6 Double
                           ,  RGBAImage -> DIM6 -> Histogram DIM6 Float
                           ,  RGBImage  -> DIM5 -> Histogram DIM5 Int32
                           ,  RGBImage  -> DIM5 -> Histogram DIM5 Double
                           ,  RGBImage  -> DIM5 -> Histogram DIM5 Float  #-}
{-# INLINABLE histogram2D #-}

-- Reshaping -------------------------------------------------------------------

-- | Reduces a 2D histogram to its linear representation. See 'resize' for a
-- reduction of the number of bins of an histogram.
-- > histogram == reduce . histogram2D
reduce :: (HistogramShape sh, Storable a, Num a)
       => Histogram (sh :. Int :. Int) a -> Histogram sh a
reduce !(Histogram sh vec) =
    let !(sh' :. h :. w) = sh
        !len2D = h * w
        !vec' = V.unfoldrN (shapeLength sh') step vec
        step !rest = let (!channels, !rest') = V.splitAt len2D rest
                     in Just (V.sum channels, rest')
    in Histogram sh' vec'
{-# SPECIALIZE reduce :: Histogram DIM5 Int32  -> Histogram DIM3 Int32
                      ,  Histogram DIM5 Double -> Histogram DIM3 Double
                      ,  Histogram DIM5 Float  -> Histogram DIM3 Float
                      ,  Histogram DIM3 Int32  -> Histogram DIM1 Int32
                      ,  Histogram DIM3 Double -> Histogram DIM1 Double
                      ,  Histogram DIM3 Float  -> Histogram DIM1 Float #-}
{-# INLINABLE reduce #-}

-- | Resizes an histogram to another index shape. See 'reduce' for a reduction
-- of the number of dimensions of an histogram.
resize :: (HistogramShape sh, Storable a, Num a)
       => Histogram sh a -> sh -> Histogram sh a
resize !(Histogram sh vec) !sh' =
    let initial = V.replicate (shapeLength sh') 0
        -- TODO: In this scheme, indexes are computed for each bin of the
        -- original histogram. It's sub-optimal as some parts of those indexes
        -- (lower dimensions) don't change at each bin.
        reIndex = toLinearIndex sh' . toBin sh' sh . fromLinearIndex sh
        ixs = V.map reIndex $ V.enumFromN 0 (shapeLength sh)
    in Histogram sh' (V.accumulate_ (+) initial ixs vec)

-- Normalisation ---------------------------------------------------------------

-- | Computes the cumulative histogram of another single dimension histogram.
-- @C(i) = SUM H(j)@ for each @j@ in @[0..i]@ where @C@ is the cumulative
-- histogram, and @H@ the original histogram.
cumulative :: (Storable a, Num a) => Histogram DIM1 a -> Histogram DIM1 a
cumulative (Histogram sh vec) = Histogram sh (V.scanl1' (+) vec)
{-# SPECIALIZE cumulative :: Histogram DIM1 Int32  -> Histogram DIM1 Int32
                          ,  Histogram DIM1 Double -> Histogram DIM1 Double
                          ,  Histogram DIM1 Float  -> Histogram DIM1 Float #-}
{-# INLINABLE cumulative #-}

-- | Normalizes the histogram so that the sum of the histogram bins is equal to
-- the given value (normalisation by the @L1@ norm).
-- This is useful to compare two histograms which have been computed from images
-- with a different number of pixels.
normalize :: (Storable a, Real a, Storable b, Fractional b)
          => Histogram sh a -> b -> Histogram sh b
normalize !hist@(Histogram _ vec) norm =
    let !ratio = norm / realToFrac (V.sum vec)
        equalizeVal !val = realToFrac val * ratio
        {-# INLINE equalizeVal #-}
    in map equalizeVal hist
{-# SPECIALIZE normalize :: Histogram sh Int32  -> Double -> Histogram sh Double
                         ,  Histogram sh Int32  -> Float  -> Histogram sh Float
                         ,  Histogram sh Double -> Double -> Histogram sh Double
                         ,  Histogram sh Double -> Float  -> Histogram sh Float
                         ,  Histogram sh Float  -> Double -> Histogram sh Double
                         ,  Histogram sh Float  -> Float  -> Histogram sh Float
                         #-}
{-# INLINABLE normalize #-}

-- | Equalizes a single channel image by equalising its histogram.
-- The algorithm equalizes the brightness and increases the contrast of the
-- image by mapping each pixels values to the value at the index of the
-- cumulative @L1@-normalized histogram :
-- @N(x, y) = H(I(x, y))@ where @N@ is the equalized image, @I@ is the image and
-- @H@ the cumulative of the histogram normalized over a @L1@ norm equals to the
-- index range of the image (@255@ for a greyscale image).
-- See <https://en.wikipedia.org/wiki/Histogram_equalization>
equalizeImage :: (FromFunction i, Integral (ImagePixel i)
                 , ToHistogram (ImagePixel i)
                 , PixelValueSpace (ImagePixel i) ~ DIM1) => i -> i
equalizeImage img =
    I.map equalizePixel img
  where
    hist            = histogram img Nothing             :: Histogram DIM1 Int32
    Z :. nBins      = shape hist
    cumNormalized   = cumulative $ normalize hist (double nBins)
    !cumNormalized' = map round cumNormalized           :: Histogram DIM1 Int32
    equalizePixel !val = fromIntegral $ cumNormalized' `index` ix1 (int val)
    {-# INLINE equalizePixel #-}
-- FIXME: GHC 7.6.3 fails to specialize
{-# SPECIALIZE equalizeImage :: GreyImage -> GreyImage #-} 
{-# INLINABLE equalizeImage #-}

-- Comparisons -----------------------------------------------------------------

-- | Computes the /Pearson\'s correlation coefficient/ between each
-- corresponding bins of the two histograms.
-- A value of 1 implies a perfect correlation, a value of -1 a perfect
-- opposition and a value of 0 no correlation at all.
-- @corr = SUM  [ (H1(i) - µ(H1)) (H1(2) - µ(H2)) ]
--       / (   SQRT [ SUM [ (H1(i) - µ(H1))^2 ] ]
--           * SQRT [ SUM [ (H2(i) - µ(H2))^2 ] ] )@
-- Where @µ(H)@ is the average value of the histogram @H@.
-- See <http://en.wikipedia.org/wiki/Pearson_correlation_coefficient>.
compareCorrel :: (Shape sh, Storable a, Real a, Storable b, Eq b, Floating b)
              => Histogram sh a -> Histogram sh a -> b
compareCorrel (Histogram sh1 vec1) (Histogram sh2 vec2)
    | sh1 /= sh2     = error "Histograms are not of equal size."
    | denominat == 0 = 1
    | otherwise      = numerat / denominat
  where
    numerat   = V.sum $ V.zipWith (*) diff1 diff2
    denominat =   sqrt (V.sum (V.map square diff1))
                * sqrt (V.sum (V.map square diff2))

    diff1 = V.map (\v1 -> realToFrac v1 - avg1) vec1
    diff2 = V.map (\v2 -> realToFrac v2 - avg2) vec2

    (avg1, avg2) = (avg vec1, avg vec2)
    avg !vec = realToFrac (V.sum vec) / realToFrac (V.length vec)
{-# SPECIALIZE compareCorrel
    :: Shape sh => Histogram sh Int32  -> Histogram sh Int32  -> Double
    ,  Shape sh => Histogram sh Int32  -> Histogram sh Int32  -> Float
    ,  Shape sh => Histogram sh Double -> Histogram sh Double -> Double
    ,  Shape sh => Histogram sh Double -> Histogram sh Double -> Float
    ,  Shape sh => Histogram sh Float  -> Histogram sh Float  -> Double
    ,  Shape sh => Histogram sh Float  -> Histogram sh Float  -> Float  #-}
{-# INLINABLE compareCorrel #-}

-- | Computes the Chi-squared distance between two histograms.
-- A value of 0 indicates a perfect match.
-- @d(i) = 2 * ((H1(i) - H2(i))^2 / (H1(i) + H2(i)))@
-- @chi = SUM (d(i))@ for each indice @i@ of the histograms.
compareChi :: (Shape sh, Storable a, Real a, Storable b, Fractional b)
           => Histogram sh a -> Histogram sh a -> b
compareChi (Histogram sh1 vec1) (Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  = (V.sum $ V.zipWith step vec1 vec2) * 2
  where
    step !v1 !v2 = let !denom = v1 + v2
                   in if denom == 0
                        then 0
                        else realToFrac (square (v1 - v2)) / realToFrac denom
    {-# INLINE step #-}
{-# SPECIALIZE compareChi
    :: Shape sh => Histogram sh Int32  -> Histogram sh Int32  -> Double
    ,  Shape sh => Histogram sh Int32  -> Histogram sh Int32  -> Float
    ,  Shape sh => Histogram sh Double -> Histogram sh Double -> Double
    ,  Shape sh => Histogram sh Double -> Histogram sh Double -> Float
    ,  Shape sh => Histogram sh Float  -> Histogram sh Float  -> Double
    ,  Shape sh => Histogram sh Float  -> Histogram sh Float  -> Float  #-}
{-# INLINABLE compareChi #-}

-- | Computes the intersection of the two histograms.
-- The higher the score is, the best the match is.
-- @intersec = SUM min(H1(i), H2(i))@ for each indice @i@ of the histograms.
compareIntersect :: (Shape sh, Storable a, Num a, Ord a)
                 => Histogram sh a -> Histogram sh a -> a
compareIntersect (Histogram sh1 vec1) (Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  = V.sum $ V.zipWith min vec1 vec2
{-# SPECIALIZE compareIntersect
    :: Shape sh => Histogram sh Int32  -> Histogram sh Int32  -> Int32
    ,  Shape sh => Histogram sh Double -> Histogram sh Double -> Double
    ,  Shape sh => Histogram sh Float  -> Histogram sh Float  -> Float #-}
{-# INLINABLE compareIntersect #-}

-- | Computed the /Earth mover's distance/ between two histograms.
-- Current algorithm only supports histograms of one dimension.
-- See <https://en.wikipedia.org/wiki/Earth_mover's_distance>
compareEMD :: (Num a, Storable a)
           => Histogram DIM1 a -> Histogram DIM1 a -> a
compareEMD hist1@(Histogram sh1 _) hist2@(Histogram sh2 _)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  = let Histogram _ vec1 = cumulative hist1
                       Histogram _ vec2 = cumulative hist2
                   in V.sum $ V.zipWith (\v1 v2 -> abs (v1 - v2)) vec1 vec2
{-# SPECIALIZE compareEMD
    :: Histogram DIM1 Int32  -> Histogram DIM1 Int32  -> Int32
    ,  Histogram DIM1 Double -> Histogram DIM1 Double -> Double
    ,  Histogram DIM1 Float  -> Histogram DIM1 Float  -> Float #-}
{-# INLINABLE compareEMD #-}

square :: Num a => a -> a
square a = a * a

double :: Integral a => a -> Double
double= fromIntegral
int :: Integral a => a -> Int
int = fromIntegral
