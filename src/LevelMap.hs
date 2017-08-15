module LevelMap (
  LevelMap (..),
  loadMap,
  Point (..),
  addPoint,
  isOnDisplayableMap,
  getCurrentDisplay,
  getCellAt)
where

data Point = Point {y :: Int, x :: Int} deriving (Show) -- To represent a point on the map

data LevelMap = LevelMap {levelMap :: [[String]],
  currul :: Point, -- Current upper left corner of the displayed area
  currbl :: Point, -- Current bottom right corner of the displayed area
  maxyx :: Point -- Height and width of the map
}

loadMap :: String -> IO LevelMap
loadMap file = do
  let file_map = map (++ ["\n"])$ map words $lines file
  return (LevelMap file_map (Point 0 0) (Point 0 0) (Point (length file_map) (getmaxLength file_map)))

getmaxLength :: [[a]] -> Int
getmaxLength [] = 0
getmaxLength (x:xs) = max (length x) (getmaxLength xs)

addPoint :: Point -> Point -> Point
addPoint (Point y1 x1) (Point y2 x2) = Point (y1 + y2) (x1 + x2)

-- Return true if the point is on the map
isOnDisplayableMap :: LevelMap -> Point -> Bool
isOnDisplayableMap (LevelMap _ (Point cy cx) (Point sy sx) (Point maxy maxx)) (Point y x) = (x>=0) && (y>=0) && (x < maxx) && (y <maxy)

-- Reduce if possible the map to a map of (height,width) starting at (starty,startx)
getCurrentDisplay :: [[String]] -> Point -> Point -> [[String]]
getCurrentDisplay tab (Point starty startx) (Point height width) = take height $ map (take width) $ drop starty $ map (drop startx) tab

getCellAt :: [[a]] -> Point -> a
getCellAt tab (Point y x) = (tab !! y) !! x

--getCharacterPos :: LevelMap -> Point
--getCharacterPos
