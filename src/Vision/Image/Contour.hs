{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , CPP
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , FlexibleInstances
           , MultiWayIf #-}
-- | Contour tracing of binary images (zero ~ background, nonzero ~ object).
--
-- Terminology:
--
-- A binary image is an image in which the pixel is boolean (represented
-- here  using 'Grey' and zero or non-zero pixel values).
--
-- All zero-value pixels are part of the "background".
--
-- An object is an connected group of non-zero pixels.
--
-- A 'Contour' is a trace of an objects' outer or inner edges.  Some
-- objects are solid, having no inner contours (consider a filled circle,
-- or letters such as 'h', 's', 'k' and 'l').  Other objects have "holes", also known as inner
-- contours.  The letters 'a' and 'e' have one hole while the letter 'B' has two.
--
-- After obtaining a 'Contours' structure (via the 'contours' function) the
-- raw traces ('Contour' type) can be used for further processing or the contours can be
-- filtered by aspects of interest and selectively re-drawn ('drawContour') , perhaps used to
-- mask the original image.
--
-- About Holes:
--
-- In cases where there is only one hole it is uniquely recorded in the
-- 'Contours' structure.  Objects with more than one hole record all inner
-- contours in one vector making them hard to extract separately - this is
-- due to the main data structure not being rich enough to record the holes
-- separately. As of writing, this is not seen as an issue because the
-- desired operation, drawContour, can still be achieved.  Changing this
-- behavior should be trivial if desired.
--
-- Use:
--
-- To use this library it is advised that you preprocess the image,
-- including thresholding (ex: 'otsu' on a grey scale image), to obtain a binary image then call:
--
-- @
-- cs = contours img
-- @
--
-- The 'Contours' structure can be accessed directly if desired.  It
-- includes an 'Map' of all contours (numbered counting from 1) and
-- a vector of the contour sizes (indexed by contour number, zero index is
-- unused/zero).
--
-- The algorithm implemented in this module follows the design laid out in
-- 'A Linear-Time Component-Labeling Algorithm Using Contour Tracing Technique' [1].
--
-- [1] http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.6330&rep=rep1&type=pdf
module Vision.Image.Contour (
    -- * Main Interface
      Contours(..), ContourId, OneContour, ContourValue, Contour(..), RowContour
    , contours
    -- * ADT style interface (hides 'Contours' internals)
    , allContourIds, lookupContour, rowContour, contourSize, contourPerimeter
    -- * Reconstructing Image Elements
    , ContourDrawStyle(..), drawContour, drawContours
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad (when)
import Control.Monad.ST
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.List (groupBy,sort)
import Data.Function (on)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import Foreign.Storable

import Vision.Image.Mutable (MutableManifest, new', write)
import qualified Vision.Image.Mutable as Mut
import Vision.Image.Grey (Grey,GreyPixel)
import Vision.Image.Type (Delayed)
import Vision.Image.Class (
      MaskedImage (..), Image (..), FromFunction (..), index
    )
import Vision.Primitive (
      Z (..), (:.) (..), Point, ix2, Size
    )

--------------------------------------------------------------------------------
--  Types and ADT-Style Interface

-- | Contours of an image include:
--    * A map from contour number to outer points and negative contour number of inner contour points.
--    * A vector of sizes for each contour for domain [1..size contourOutlines] (the zero index is meaningless)
data Contours =
        Contours { contourOutlines :: Map ContourId Contour
                 , contourSizes    :: !(VU.Vector Int)
                 }

allContourIds :: Contours -> [ContourId]
allContourIds = Map.keys . contourOutlines

contourPerimeter :: ContourId -> Contours -> [Point]
contourPerimeter i m =
    maybe [] (map fst . VU.toList . outerContour) (lookupContour i m)

contourSize :: Contours -> ContourId -> Int
contourSize (Contours _ s) i
    | unCID i < 0 || unCID i >= VU.length s = 0
    | otherwise                             = s VU.! unCID i

lookupContour :: ContourId -> Contours -> Maybe Contour
lookupContour i m = Map.lookup i (contourOutlines m)

-- |Contours are identified by a numeric ID number.
newtype ContourId = CID { unCID :: Int } deriving (Eq, Ord, Storable, Num, Show)

-- |A contour is described by the points on the perimeter and a boolean
-- indicating if that point is "terminal" (next pixel to
-- the right is background iff the point is terminal).  The terminal
-- information allows for a slightly simpler 'drawContour' implementation.
type OneContour    = VU.Vector ContourValue
type ContourValue  = (Point,Bool)
data Contour = Contour { outerContour  :: OneContour
                       , innerContours :: [OneContour]
                       } -- Pair of outer and inner contours

insOuterContour :: ContourId -> OneContour -> Map ContourId Contour
                                           -> Map ContourId Contour
insOuterContour cid o mp =
    let c = Contour o []
    in Map.insert cid c mp

insInnerContour :: ContourId -> OneContour -> Map ContourId Contour
                                           -> Map ContourId Contour
insInnerContour cid i mp =
    let c = Contour (error "Impossible: Inner contour with no outer!") [i]
        f _ (Contour o is) = Contour o (i:is)
    in Map.insertWith f cid c mp

-- |RowContour is a method of expressing contours by, for each row,
-- recording the start of an object and the end (due to reaching the other
-- side or a hole/inner contour) for each row.
type RowContour = VU.Vector (Point,Point)

--------------------------------------------------------------------------------
--  Image Reconstruction

-- | Outline: Just draw the edge.
--
-- OuterOutline: Outline the outer contours only, no hole contours
-- AllOutlines: Draw all contours
-- Fill: Draw the object but fill it in, ignoring holes.
-- FillWithHoles: Draw the object and do not fill in the holes.
data ContourDrawStyle = OuterOutline | AllOutlines | Fill | FillWithHoles
      deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Draws a given contour. The size specified must be large enough to
-- include the coordinate originally occupied by the contour being drawn,
-- no cropping or other transformation is done.
drawContour :: Contours -> ContourDrawStyle -> ContourId -> Size -> Grey
drawContour master sty c sz = drawContours master sty [c] sz

-- |Draws many contours.  See 'drawContour'.
drawContours :: Contours -> ContourDrawStyle -> [ContourId] -> Size -> Grey
drawContours m AllOutlines ids sz = drawOutlines listOfUVec m ids sz
 where listOfUVec (Contour o is) = o:is
drawContours m OuterOutline ids sz = drawOutlines listOfUVec m ids sz
 where listOfUVec (Contour o _) = [o]
drawContours m sty ids sz = drawRows pnts sz
 where lk = (`lookupContour` m)
       pnts = case sty of
                  Fill          -> map (VU.toList . outerContour) $ catMaybes $ map lk ids -- map (map (\(a,x) -> (a,not x)) . lk) innerIds ++ map lk outerIds
                  FillWithHoles -> map  (concatMap VU.toList . maybe [] (\x -> outerContour x : innerContours x) . lk) ids
                  _             -> error "Impossible: Style is not Fill, FillWithHoles"

drawOutlines :: (Contour -> [VU.Vector ContourValue]) -> Contours -> [ContourId] -> Size -> Grey
drawOutlines oper m ids sz = runST f
 where
  f = do
    i <- new' sz 0 :: ST s (MutableManifest GreyPixel s)
    let vs = map fst $ concatMap VU.toList $ concatMap oper $ catMaybes $ map (`lookupContour` m) ids
    mapM_ (\p -> write i p 255) vs
    Mut.unsafeFreeze i


-- | Draws rows, throwing an exception when the size is too small for the
-- coordinates.
drawRows :: [[ContourValue]] -> Size -> Grey
drawRows vs sz = runST $ do
    i <- new' sz 0
    mapM_ (drawMutable i) vs
    Mut.unsafeFreeze i

drawMutable :: MutableManifest GreyPixel s -> [ContourValue] -> ST s ()
drawMutable i cs = VU.mapM_ (f i) rs
 where
     rs = rowContour cs
     f img (start,stop) = go (start, stop)
       where go (s@(Z:.row:.col),t) = do
                write img s 255
                when (s /= t) $ go (Z :. row :. (col+1),t)

-- |Given a vector including outer (and optionally inner) contour points,
-- make 'row contour' from which is easier to transform back into a binary
-- image.  By not including the inner contour points the row will be filled, making
-- traces of objects with holes appear solid.
rowContour :: [ContourValue] -> RowContour
rowContour cs =
    let rows :: [[(Point,Bool)]]
        rows = groupBy ((==) `on` ((\(Z:.r:._) -> r) . fst)) $ sort cs -- XXX consider vector quick/tim sort
    in VU.fromList $ concatMap walkM rows
 where
  walkM :: [(Point,Bool)] -> [(Point,Point)]
  walkM [x] = [(fst x,fst x)]
  walkM x   = maybe (error $ "Impossible: No terminal when walking contour: " ++ show (x,cs)) id $ walk x
  walk :: [(Point,Bool)] -> Maybe [(Point,Point)]
  walk [] = Just []
  walk xs@(x:_) = case dropWhile (not . snd) xs of
                      []     -> Nothing
                      (t:ys) -> ((fst x,fst t) :) <$> walk ys

-- |The meat of this module is the 'contours' function, which extracts
-- the contours (outer and inner outlines) of a binary image.
-- Zero-valued pixels are the background and non-zero are active/objects to
-- trace.  The output, 'Contours', contains enough information to determine
-- the number of contours, their traces, the size in pixels (filled size
-- and perimeter), number of holes, etc.
contours :: (Image src, Num (ImagePixel src), Eq (ImagePixel src)) => src -> Contours
contours src = runST $ do
     let bsrc = fromFunction (Z :. y+2 :. x+2) mkBorder
     mutImg   <- new' (shape bsrc) zid
     (outlines,sz) <- doLabeling bsrc mutImg
     sizes <- freezeBlobSizes sz
     return (Contours outlines sizes)
 where
 (Z :. y :. x) = shape src

 mkBorder (Z :. j :. i)
   | j == 0 || j == (y+1) || i == 0 || i == (x+1) = background
   | otherwise                                    = index src (Z :. j-1 :. i-1)

-- The image is assumed to be binary and should have values of either 0 (black) or... nonzero (white)
-- here we assume the background is black.  Nonzero would require more
-- change elsewhere!
background :: Num a => a
background = 0

zid :: ContourId
zid = CID 0

data BlobSizes s = BS (VM.MVector s Int)

freezeBlobSizes :: BlobSizes s -> ST s (VU.Vector Int)
freezeBlobSizes (BS v) = VU.unsafeFreeze v

incBlobSizes :: ContourId -> BlobSizes s -> ST s (BlobSizes s)
incBlobSizes (CID i) s@(BS v)
  | i > 0 =
     if VM.length v <= i
         then do nv <- VM.unsafeGrow v (i*2)
                 mapM_ (\ix -> VM.unsafeWrite nv ix 0) [i..i*2-1]
                 VM.unsafeWrite nv i 1
                 return (BS nv)
         else do p <- VM.unsafeRead v i
                 VM.unsafeWrite v i (p+1)
                 return s
  | otherwise = return s

zeroBlobSizes :: ST s (BlobSizes s)
zeroBlobSizes = BS <$> VM.replicate 1024 0

-- Make a contour image of the same dimension but with ContourIDs instead
-- of pixels.
doLabeling :: forall s p. (Storable p, Num p, Eq p) => Delayed p -> MutableManifest ContourId s -> ST s (Map ContourId Contour,BlobSizes s)
doLabeling src mutImg = zeroBlobSizes >>= go (Just $ ix2 1 1) (CID 0) (CID 1) mempty
 where
 getCID    :: Point -> ST s ContourId
 getCID     = Mut.read mutImg
 setCID i c = write mutImg i c
 getPixel :: Point -> ImagePixel (Delayed p)
 getPixel   = index src

 incIx :: Point -> Maybe Point
 incIx !(Z :. (!y) :. (!x))
    | x < xMax-1  = Just $ Z :. y     :. (x+1)
    | y < yMax-1  = Just $ Z :. (y+1) :. 1
    | otherwise = Nothing

 (Z :. yMax :. xMax) = shape src

 -- Traverse the source image top to bottom, left to right.  If the pixel
 -- has an ID then propagate that ID to all the following active pixels in
 -- the row.  If the pixel is active and has no ID then trace either an
 -- inner or outer contour.  If the pixel is inactive then skip it.
 go Nothing   _ _ !mp v              = return (mp,v)
 go (Just idx) leftCID !newCID !mp v =
   do thisCID <- getCID idx
      if | val == background                     -> skipForward -- this step doesn't appear in the paper! D'oh
         | thisCID == zid && above == background ->
                         do -- Step 1: Outer contour trace (active pixel with id=0 and above is background)
                            newContour <- traceContour src mutImg ExternalContour idx newCID
                            go (Just idx) newCID (newCID + 1) (insOuterContour newCID newContour mp) v
         | below == background ->               -- Step 2: P is an              white pixel
             do belowCID <- getCID belowIdx     --                 ^          ^
                if | belowCID == zid ->         --                 ^ unmarked ^
                         do -- Step 2a: Inner contour trace, below pixel was unmarked
                            let innerCID = if zid == thisCID then leftCID else thisCID
                            inner <- traceContour src mutImg InternalContour idx innerCID
                            go (incIx idx) innerCID newCID (insInnerContour innerCID inner mp) v
                            -- there can be more than one inner contour, make a richer container structure than IntMap?
                            -- Notice this isn't entirely necessary, one
                            -- CID can contain all inner contours and they can be redrawn correctly.
                   | otherwise -> stepForward -- Step 2b: Previously-observed contour
         | otherwise                             -> stepForward -- Active pixel not on a contour
   where val         = getPixel idx
         above       = getPixel (Z :. y-1 :. x)
         below       = getPixel belowIdx
         belowIdx    = Z :. y+1 :. x
         Z :. y :. x = idx
         stepForward = do xId <- if leftCID <= zid
                                  then getCID idx
                                  else return leftCID
                          setCID idx xId
                          nv <- incBlobSizes xId v
                          go (incIx idx) xId newCID mp nv
         skipForward = go (incIx idx) (-2) newCID mp v

-- Mark surrounding background pixels
-- label non-background pixels with CID
--
-- Unroll the loop one step to account for the lone-pixel case.  Without
-- lone pixels the tight inner loop can save a check (See 'Impossible')
--
-- TODO: optimize later, duplicate tracer and remove the p==pos, after a benchmarking method is setup.
traceContour :: forall p s. (Storable p, Num p, Eq p) => Delayed p -> MutableManifest ContourId s -> ContourType -> Point -> ContourId -> ST s OneContour
traceContour src mutImg contourTy origPnt assignedCID =
  do next <- tracer origPnt startPos
     case next of
         Nothing              -> return (VU.fromList $ fixList [(origPnt,True)])
         Just (sndPnt,sndPos) -> do
            let f pnt pos = do (nPnt,nPos) <- maybe (error "Impossible: Nothing in inner") id <$> tracer pnt pos
                               if pnt == origPnt && nPnt == sndPnt
                                   then return [] -- XXX some algorithms duplicate the start point, pnt, as the last point `return [pnt]`.  Should we?
                                   else ((pnt,terminal pnt) :) <$> f nPnt nPos
            VU.fromList . fixList . ((origPnt, terminal origPnt):) <$> f sndPnt sndPos

 where
   terminal (Z :. row :. col) = 0 == getPixel (Z :. row :. (col+1))
   -- Translate between indexes in our border-added image and the original
   fixList xs = let f (Z :. a :. b, t) = (Z :. a-1 :. b-1,t) in map f xs
   startPos   = case contourTy of { ExternalContour -> UR ; InternalContour -> LL  }
   setCID i c = write mutImg i c
   getPixel :: Point -> ImagePixel (Delayed p)
   getPixel   = index src

   {-# INLINE tracer #-}
   tracer pnt pos =
       let tracer' True p | p == pos = setCID pnt assignedCID >> return Nothing
           tracer' _ p = do let rpnt = relPoint pnt p
                                v    = getPixel rpnt
                            if | v == background -> do setCID rpnt (-1)
                                                       tracer' True (incCP p)
                               | otherwise       -> do setCID pnt assignedCID
                                                       return (Just (rpnt, decCP2 p))
       in tracer' False pos


--------------------------------------------------------------------------------
--  Internal Types and Utilities

data ContourType = ExternalContour | InternalContour deriving (Eq)

-- A contour position is Upper/Lower/Middle Left/Center/Right pixel
-- relative to the current.  Because we add a border to the image prior
-- to processing, all original pixels `cross` contours positions are
-- a valid point.
data ContourPos  = MR | LR | LC | LL
                 | ML | UL | UC | UR
            deriving (Enum, Bounded, Eq, Show)

relPoint :: Point -> ContourPos -> Point
relPoint (Z :. row :. col) pos = Z :. row' :. col'
 where !row' = row + y
       !col' = col + x
       x = colOffset VU.! fromEnum pos
       y = rowOffset VU.! fromEnum pos

-- Rather than have branching (and expected poor performance),
-- use a table of x/y offsets for the relative pixel position.
colOffset,rowOffset :: VU.Vector Int
rowOffset = VU.fromList [0,1,1,1,0,-1,-1,-1]
colOffset = VU.fromList [1,1,0,-1,-1,-1,0,1]

-- Position clockwise by one tick
incCP :: ContourPos -> ContourPos
incCP  = toEnum . ((`rem` 8) . (+ 1)) . fromEnum

-- Position counter-clockwise by two ticks
decCP2 :: ContourPos -> ContourPos
decCP2 = toEnum . ((`rem` 8) . (+ 6)) . fromEnum
