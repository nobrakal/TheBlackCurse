module LevelMap (
  LevelMap (..),
  loadMap,
  Point (..),
  addPoint,
  isOnDisplayableMap)
where

data Point = Point {y :: Int, x :: Int} deriving (Show) -- To represent a point on the map


data LevelMap = LevelMap {levelMap :: [[Char]],
  currul :: Point, -- Current upper left corner of the displayed area
  currbl :: Point, -- Current bottom right corner of the displayed area
  maxyx :: Point -- Height and width of the map
}

loadMap :: FilePath -> IO LevelMap
loadMap path = do
  file <- (readFile path)
  let file_map = map (++ "\n") $lines file
  return (LevelMap file_map (Point 0 0) (Point 0 0) (Point (length file_map) (getmaxLength file_map)))

getmaxLength :: [[a]] -> Int
getmaxLength [] = 0
getmaxLength (x:xs) = max (length x) (getmaxLength xs)

addPoint :: Point -> Point -> Point
addPoint (Point y1 x1) (Point y2 x2) = Point (y1 + y2) (x1 + x2)

-- Return true if the point is on the map
isOnDisplayableMap :: LevelMap -> Point -> Bool
isOnDisplayableMap (LevelMap _ (Point cy cx) (Point sy sx) (Point maxy maxx)) (Point y x) = (x>=0) && (y>=0) && (x < (maxx-sx+cx)) && (y <(maxy-sy+cy))
