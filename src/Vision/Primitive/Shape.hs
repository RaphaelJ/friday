{-# LANGUAGE BangPatterns
           , CPP
           , FlexibleInstances
           , TypeFamilies, MultiParamTypeClasses, FlexibleContexts
           , TypeOperators #-}

-- | 'Shape's are similar to what you could found in @repa@. 'Shape' are used
-- both for indexes and shapes.
--
-- To create a shape/index, use the 'ix1', 'ix2', 'ix3' ... helpers :
--
-- > size = ix2 200 100
--
-- To pull values from a shape, use the 'Z' and ':.' constructors :
--
-- > Z :. h :. w = size
module Vision.Primitive.Shape (
      Shape (..), Z (..), (:.) (..)
    -- * Common dimensions.
    , DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9
    -- * Helpers
    , ix1, ix2, ix3, ix4, ix5, ix6, ix7, ix8, ix9
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Word
#endif

import Foreign.Storable (Storable (..))
import Foreign.Ptr (castPtr, plusPtr)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Generic.Mutable (MVector(..))
import qualified Data.Vector.Generic as VG

-- | Class of types that can be used as array shapes and indices.
class Eq sh => Shape sh where
    -- | Gets the number of dimensions in a shape.
    shapeRank :: sh -> Int

    -- | Gets the total number of elements in an array of this shape.
    shapeLength :: sh -> Int

    -- | Gives the first index of an array.
    shapeZero :: sh

    -- | Gives the successor of an index, given the shape of the array.
    shapeSucc :: sh -- ^ Shape of the array.
              -> sh -- ^ Index.
              -> sh

    -- | Convert an index into its equivalent flat, linear, row-major version.
    toLinearIndex :: sh  -- ^ Shape of the array.
                  -> sh  -- ^ Index into the array.
                  -> Int

    -- | Inverse of `toLinearIndex`.
    fromLinearIndex :: sh  -- ^ Shape of the array.
                    -> Int -- ^ Index into linear representation.
                    -> sh

    -- | Return the ascending list of indexes for the given shape.
    shapeList :: sh -> [sh]

    -- | Check whether an index is within a given shape.
    inShape :: sh   -- ^ Shape of the array.
            -> sh   -- ^ Index to check for.
            -> Bool

-- | An index of dimension zero.
data Z = Z deriving (Show, Read, Eq, Ord)

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head = !tail :. !head
    deriving (Show, Read, Eq, Ord)

newtype instance VU.MVector s Z = MV_Z (VU.MVector s ())
newtype instance VU.Vector    Z = V_Z  (VU.Vector    ())

instance MVector VU.MVector Z where
  {-# INLINE basicLength #-}
  basicLength (MV_Z v)          = basicLength v
  basicUnsafeSlice s e (MV_Z v) = MV_Z $ basicUnsafeSlice s e v
  basicUnsafeRead (MV_Z v) i    = basicUnsafeRead  v i >>= \_ -> return Z
  basicUnsafeNew   i            = MV_Z `fmap` basicUnsafeNew i
  basicUnsafeWrite (MV_Z v) i a = a `seq` basicUnsafeWrite v i ()
  basicOverlaps (MV_Z a) (MV_Z b) = basicOverlaps a b

instance VG.Vector VU.Vector Z where
  {-# INLINE basicLength #-}
  basicLength (V_Z v)          = VG.basicLength v
  basicUnsafeFreeze (MV_Z v)   = V_Z `fmap` VG.basicUnsafeFreeze v
  basicUnsafeThaw (V_Z v)      = MV_Z `fmap` VG.basicUnsafeThaw v
  basicUnsafeSlice s e (V_Z v) = V_Z $ VG.basicUnsafeSlice s e v
  basicUnsafeIndexM (V_Z v) i  = VG.basicUnsafeIndexM v i >>= \_ -> return Z

instance Unbox Z

newtype instance VU.MVector s (t :. h) = MV_Dim (VU.MVector s (t , h))
newtype instance VU.Vector    (t :. h) = V_Dim  (VU.Vector    (t , h))

instance (Unbox t, Unbox h) => MVector VU.MVector (t :. h) where
  {-# INLINE basicLength #-}
  basicLength (MV_Dim v)          = basicLength v
  basicUnsafeSlice s e (MV_Dim v) = MV_Dim $ basicUnsafeSlice s e v
  basicUnsafeRead (MV_Dim v) i    = pairToPoint `fmap` basicUnsafeRead  v i
  basicUnsafeNew   i              = MV_Dim `fmap` basicUnsafeNew i
  basicUnsafeWrite (MV_Dim v) i a = basicUnsafeWrite v i (pointToPair a)
  basicOverlaps (MV_Dim a) (MV_Dim b) = basicOverlaps a b

instance (Unbox t, Unbox h) => VG.Vector VU.Vector (t :. h) where
  {-# INLINE basicLength #-}
  basicLength (V_Dim v) = VG.basicLength v
  basicUnsafeFreeze (MV_Dim v)   = V_Dim `fmap` VG.basicUnsafeFreeze v
  basicUnsafeThaw (V_Dim v)      = MV_Dim `fmap` VG.basicUnsafeThaw v
  basicUnsafeSlice s e (V_Dim v) = V_Dim $ VG.basicUnsafeSlice s e v
  basicUnsafeIndexM (V_Dim v) i  = pairToPoint `fmap` VG.basicUnsafeIndexM v i

instance (Unbox t, Unbox h) => Unbox (t :. h)

pairToPoint :: (tail, head) -> tail :. head
pairToPoint (a,b) = a :. b

pointToPair :: tail :. head -> (tail, head)
pointToPair (a :. b) = (a,b)


-- Common dimensions.
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int
type DIM4 = DIM3 :. Int
type DIM5 = DIM4 :. Int
type DIM6 = DIM5 :. Int
type DIM7 = DIM6 :. Int
type DIM8 = DIM7 :. Int
type DIM9 = DIM8 :. Int

instance Shape Z where
    shapeRank Z = 0
    {-# INLINE shapeRank #-}

    shapeLength Z = 1
    {-# INLINE shapeLength #-}

    shapeZero = Z
    {-# INLINE shapeZero #-}

    shapeSucc _ _= Z
    {-# INLINE shapeSucc #-}

    toLinearIndex Z _ = 0
    {-# INLINE toLinearIndex #-}

    fromLinearIndex Z _ = Z
    {-# INLINE fromLinearIndex #-}

    -- | Returns every shape in ascending order
    --
    -- > shapeList sh = map fromLinearIndex [1..shapeLength sh - 1]
    shapeList Z = [Z]
    {-# INLINE shapeList #-}

    inShape Z Z = True
    {-# INLINE inShape #-}

instance Storable Z where
    sizeOf _ = 0
    {-# INLINE sizeOf #-}

    alignment _ = 0
    {-# INLINE alignment #-}

    peek _ = return Z
    {-# INLINE peek #-}

    poke _ _ = return ()
    {-# INLINE poke #-}

instance Shape sh => Shape (sh :. Int) where
    shapeRank (sh  :. _) = shapeRank sh + 1
    {-# INLINE shapeRank #-}

    shapeLength (sh :. n) = shapeLength sh * n
    {-# INLINE shapeLength #-}

    shapeZero = shapeZero :. 0
    {-# INLINE shapeZero #-}

    shapeSucc (sh :. n) (sh' :. ix)
        | ix' >= n  = shapeSucc sh sh' :. 0
        | otherwise = sh'              :. ix'
      where
        !ix' = ix + 1
    {-# INLINE shapeSucc #-}

    toLinearIndex (sh :. n) (sh' :. ix) =   toLinearIndex sh sh' * n
                                          + ix
    {-# INLINE toLinearIndex #-}

    fromLinearIndex (sh :. n) ix
        | shapeRank sh == 0 = fromLinearIndex sh 0 :. ix
        | otherwise         = let (q, r) = ix `quotRem` n
                              in fromLinearIndex sh q :. r
    {-# INLINE fromLinearIndex #-}

    shapeList (sh :. n) = [ sh' :. i | sh' <- shapeList sh, i <- [0..n-1] ]
    {-# INLINE shapeList #-}

    inShape (sh :. n) (sh' :. ix) = word ix < word n && inShape sh sh'
    {-# INLINE inShape #-}

instance Storable sh => Storable (sh :. Int) where
    sizeOf ~(sh :. _) = sizeOf (undefined :: Int) + sizeOf sh
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Int)
    {-# INLINE alignment #-}

    peek !ptr = do
        let !ptr' = castPtr ptr
        (:.) <$> peek (castPtr $! ptr' `plusPtr` sizeOf (undefined :: Int)) <*> peek ptr'
    {-# INLINE peek #-}

    poke !ptr (sh :. n) = do
        let !ptr' = castPtr ptr
        poke (castPtr $! ptr' `plusPtr` sizeOf n) sh >> poke ptr' n
    {-# INLINE poke #-}

-- | Helper for index construction.
--
-- Use this instead of explicit constructors like @(Z :. (x :: Int))@
-- The this is sometimes needed to ensure that 'x' is constrained to
-- be in @Int@.
ix1 :: Int -> DIM1
ix1 x = Z :. x
{-# INLINE ix1 #-}

ix2 :: Int -> Int -> DIM2
ix2 y x = Z :. y :. x
{-# INLINE ix2 #-}

ix3 :: Int -> Int -> Int -> DIM3
ix3 z y x = Z :. z :. y :. x
{-# INLINE ix3 #-}

ix4 :: Int -> Int -> Int -> Int -> DIM4
ix4 a z y x = Z :. a :. z :. y :. x
{-# INLINE ix4 #-}

ix5 :: Int -> Int -> Int -> Int -> Int -> DIM5
ix5 b a z y x = Z :. b :. a :. z :. y :. x
{-# INLINE ix5 #-}

ix6 :: Int -> Int -> Int -> Int -> Int -> Int -> DIM6
ix6 c b a z y x = Z :. c :. b :. a :. z :. y :. x
{-# INLINE ix6 #-}

ix7 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> DIM7
ix7 d c b a z y x = Z :. d :. c :. b :. a :. z :. y :. x
{-# INLINE ix7 #-}

ix8 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> DIM8
ix8 e d c b a z y x = Z :. e :. d :. c :. b :. a :. z :. y :. x
{-# INLINE ix8 #-}

ix9 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> DIM9
ix9 f e d c b a z y x = Z :. f :. e :. d :. c :. b :. a :. z :. y :. x
{-# INLINE ix9 #-}

word :: Integral a => a -> Word
word = fromIntegral
