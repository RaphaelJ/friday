module Vision.Primitive.Shape (
      Shape (..), Z (..), (:.) (..)
    -- * Common dimensions.
    , DIM0, DIM1, DIM2, DIM3, DIM4, DIM5
    -- * Helpers
    ,ix1, ix2, ix3, ix4, ix5
) where

-- Shape -----------------------------------------------------------------------

-- | Class of types that can be used as array shapes and indices.
class Eq sh => Shape sh where
    -- | Gets the number of dimensions in a shape.
    rank :: sh -> Int

    -- | Gets the total number of elements in an array with this shape.
    size :: sh -> Int

    -- | Convert an index into its equivalent flat, linear, row-major version.
    toIndex :: sh  -- ^ Shape of the array.
            -> sh  -- ^ Index into the array.
            -> Int

    -- | Inverse of `toIndex`.
    fromIndex :: sh  -- ^ Shape of the array.
              -> Int -- ^ Index into linear representation.
              -> sh

    -- | Check whether an index is within a given shape.
    inShape :: sh   -- ^ Shape of the array.
            -> sh   -- ^ Index to check for.
            -> Bool

-- | An index of dimension zero
data Z = Z deriving (Show, Read, Eq, Ord)

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head = !tail :. !Int
    deriving (Show, Read, Eq, Ord)

-- Common dimensions
type DIM0       = Z
type DIM1       = DIM0 :. Int
type DIM2       = DIM1 :. Int
type DIM3       = DIM2 :. Int
type DIM4       = DIM3 :. Int
type DIM5       = DIM4 :. Int

instance Shape Z where
    rank Z = 0
    {-# INLINE rank #-}

    size Z = 1
    {-# INLINE size #-}

    toIndex Z z = 0
    {-# INLINE toIndex #-}

    fromIndex Z _ = Z
    {-# INLINE fromIndex #-}

    inShape Z Z = True
    {-# INLINE inShape #-}

instance Shape sh => Shape (sh :. Int) where
    rank (sh  :. _) = rank sh + 1
    {-# INLINE rank #-}

    size (sh :. n) = size sh * n
    {-# INLINE size #-}

    toIndex (sh :. n) (sh' :. ix) =   toIndex sh sh' * n
                                    + ix
    {-# INLINE toIndex #-}

    fromIndex (sh :. n) ix | rank sh1 == 0 = ix
                           | otherwise     =
        let (q, r) = ix `quotRem` n
        in fromIndex sh q * n + r
    {-# INLINE fromIndex #-}

    inShape (sh :. n) (sh' :. ix) = word ix < word n && inShapeRange sh sh'
    {-# INLINE inShape #-}

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
