module Space
  ( Point (..),
  Direction (..),
  dirToPoint
  ) where

data Point = Point {y :: Int, x :: Int} deriving (Show) -- To represent a point on the map
data Direction = UP | DOWN | LEFT | RIGHT | NULL deriving (Show)

dirToPoint :: Direction -> Point
dirToPoint UP = Point (-1) 0
dirToPoint DOWN = Point 1 0
dirToPoint LEFT = Point 0 (-1)
dirToPoint RIGHT =  Point 0 1
dirToPoint _ = Point 0 0
