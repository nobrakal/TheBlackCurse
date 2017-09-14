module Space
  ( Point (..),
  Direction (..),
  dirToPoint,
  pointToDir,
  dist,
  getRadiusFromPoint,
  getListOfPoint,
  isNear,
  signumFstOrSnd
  ) where

data Point = Point {y :: Int, x :: Int} deriving (Show, Eq, Read) -- To represent a point on the map
instance Num Point where
  (Point a1 b1) + (Point a2 b2) = Point (a1+a2) (b1+b2)
  (Point a1 b1) * (Point a2 b2) = Point (a1*a2) (b1*b2)
  abs (Point a1 b1) = Point (abs a1) (abs b1)
  negate (Point a1 b1) = Point (-a1) (-b1)
  signum (Point a1 b1) = Point (signum a1) (signum b1)
  fromInteger i  = Point (fromInteger i) 0 -- Or anything else
data Direction = UP | DOWN | LEFT | RIGHT | NULL deriving (Show, Eq)

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

signumFstOrSnd :: Point -> Bool -> Point
signumFstOrSnd p@(Point x 0) _ = signum p
signumFstOrSnd p@(Point 0 x) _ = signum p
signumFstOrSnd (Point y x) b = if b then Point (signum y) 0 else Point 0 $ signum x

dist :: Point -> Point -> Int
dist (Point x y) (Point x' y') = floor . sqrt $ xx^2 + yy^2
  where
    xx = fromIntegral $ x-x'
    yy = fromIntegral $ y-y'

getRadiusFromPoint :: Point -> Int -> [Point]
getRadiusFromPoint start@(Point starty startx) radius_w = getRadiusFromPoint' start radius_w $ getListOfPoint listy listx
  where
    listy = [(starty-radius_w)..(starty+radius_w)]
    listx = [(startx-radius_w)..(startx+radius_w)]

getListOfPoint :: [Int] -> [Int] -> [Point]
getListOfPoint y = concatMap (zipWith Point y . repeat)

getRadiusFromPoint' :: Point -> Int -> [Point] -> [Point]
getRadiusFromPoint' start radius_w [] = []
getRadiusFromPoint' start radius_w (x:xs)
  | dist start x <= radius_w = x : todo
  |otherwise = todo
  where
    todo = getRadiusFromPoint' start radius_w xs

-- Test if the first point is near the second one (at his top, bottom, left or right)
isNear :: Point -> Point -> Bool
isNear (Point y1 x1) (Point y2 x2)
  | y1 == y2 && (abs (x1 - x2) == 1) = True
  | x1 == x2 && (abs (y1 - y2) == 1) = True
  | otherwise = False
