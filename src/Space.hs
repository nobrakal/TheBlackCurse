module Space
  ( Point (..),
  Direction (..),
  dirToPoint,
  pointToDir,
  addPoint
  ) where

data Point = Point {y :: Int, x :: Int} deriving (Show) -- To represent a point on the map
data Direction = UP | DOWN | LEFT | RIGHT | NULL deriving (Show)

dirToPoint :: Direction -> Point
dirToPoint UP = Point (-1) 0
dirToPoint DOWN = Point 1 0
dirToPoint LEFT = Point 0 (-1)
dirToPoint RIGHT =  Point 0 1
dirToPoint _ = Point 0 0

pointToDir :: Point -> Direction
pointToDir (Point (-1) 0 )= UP
pointToDir (Point 1 0) = DOWN
pointToDir (Point 0 (-1)) = LEFT
pointToDir (Point 0 1) = RIGHT
pointToDir _ = NULL

addPoint :: Point -> Point -> Point
addPoint (Point y1 x1) (Point y2 x2) = Point (y1 + y2) (x1 + x2)
