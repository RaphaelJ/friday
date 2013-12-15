{-# LANGUAGE BangPatterns #-}

module Vision.Image.Primitive (
      Point (..), RPoint (..), Rect (..), Size (..)
    ) where

import Data.RatioInt (RatioInt)

data Point = Point {
      pX :: {-# UNPACK #-} !Int, pY :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

-- | Rational coordinates used for interpolations.
data RPoint = RPoint {
      rpX :: {-# UNPACK #-} !RatioInt, rpY :: {-# UNPACK #-} !RatioInt
    } deriving (Show, Read, Eq, Ord)

data Rect = Rect {
      rX     :: {-# UNPACK #-} !Int, rY      :: {-# UNPACK #-} !Int
    , rWidth :: {-# UNPACK #-} !Int, rHeight :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

data Size = Size {
      sWidth :: {-# UNPACK #-} !Int, sHeight :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)
