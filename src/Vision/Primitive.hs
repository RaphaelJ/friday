{-# LANGUAGE BangPatterns #-}

module Vision.Primitive (
      module Vision.Primitive.Shape
    , Point, Size, Rect (..), RPoint (..)
    ) where

import Data.RatioInt (RatioInt)

import Vision.Primitive.Shape

type Point = DIM2

type Size = DIM2

data Rect = Rect {
      rX     :: {-# UNPACK #-} !Int, rY      :: {-# UNPACK #-} !Int
    , rWidth :: {-# UNPACK #-} !Int, rHeight :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

-- | Rational coordinates used for interpolations.
data RPoint = RPoint {
      rpX :: {-# UNPACK #-} !RatioInt, rpY :: {-# UNPACK #-} !RatioInt
    } deriving (Show, Read, Eq, Ord)
