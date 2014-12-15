{-# LANGUAGE BangPatterns #-}

module Vision.Primitive (
      module Vision.Primitive.Shape
    , Point, Size, Rect (..), RPoint (..)
    ) where

import Data.RatioInt (RatioInt)

import Vision.Primitive.Shape

-- | Coordinates inside the image.
--
-- Can be constructed using 'ix2'. The first parameter is the y coordinate while
-- the second is the x coordinate (i.e. @ix2 y x@).
type Point = DIM2

-- | Size of an object.
--
-- Can be constructed using 'ix2'. The first parameter is the height while the
-- second is the width (i.e. @ix2 h w@).
type Size = DIM2

data Rect = Rect {
      rX     :: {-# UNPACK #-} !Int, rY      :: {-# UNPACK #-} !Int
    , rWidth :: {-# UNPACK #-} !Int, rHeight :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

-- | Rational coordinates used for interpolations.
data RPoint = RPoint {
      rpX :: {-# UNPACK #-} !RatioInt, rpY :: {-# UNPACK #-} !RatioInt
    } deriving (Show, Read, Eq, Ord)
