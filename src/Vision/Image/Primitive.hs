{-# LANGUAGE BangPatterns #-}
module Vision.Image.Primitive (Rect (..), RPoint (..)) where

import Data.RatioInt (RatioInt)

data Rect = Rect {
      rX     :: {-# UNPACK #-} !Int, rY      :: {-# UNPACK #-} !Int
    , rWidth :: {-# UNPACK #-} !Int, rHeight :: {-# UNPACK #-} !Int
    } deriving (Show, Read, Eq, Ord)

-- | Rational coordinates used for interpolations.
data RPoint = RPoint {
      dpX :: {-# UNPACK #-} !RatioInt, dpY :: {-# UNPACK #-} !RatioInt
    } deriving (Show, Read, Eq, Ord)
