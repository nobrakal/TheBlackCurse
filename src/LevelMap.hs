module LevelMap (
  LevelMap (..),
  loadMap,
  isOnDisplayableMap,
  getCurrentDisplay,
  getCellAt,
  getCharPos,
  canInteractWith,
  canGoTrough,
  willSay)
where

import Space
import Data.ConfigFile

data LevelMap = LevelMap {levelMap :: [[String]],
  currul :: Point, -- Current upper left corner of the displayed area
  currbl :: Point, -- Current bottom right corner of the displayed area
  maxyx :: Point -- Height and width of the map
}

loadMap :: String -> Point -> Point -> LevelMap
loadMap file currul currbl = do
  let file_map = map (++ ["\n"])$ map words $lines file
  LevelMap file_map currul currbl (Point (length file_map) (getmaxLength file_map))

getmaxLength :: [[a]] -> Int
getmaxLength [] = 0
getmaxLength (x:xs) = max (length x) (getmaxLength xs)

-- Return true if the point is on the map
isOnDisplayableMap :: LevelMap -> Point -> Bool
isOnDisplayableMap (LevelMap _ (Point cy cx) (Point sy sx) (Point maxy maxx)) (Point y x) = (x>=0) && (y>=0) && (x < maxx) && (y <maxy)

-- Reduce if possible the map to a map of (height,width) starting at (starty,startx)
getCurrentDisplay :: [[String]] -> Point -> Point -> [[String]]
getCurrentDisplay tab (Point starty startx) (Point height width) = take height $ map (take width) $ drop starty $ map (drop startx) tab

getCellAt :: [[a]] -> Point -> a
getCellAt tab (Point y x) = (tab !! y) !! x

-- Find the "@" on the map
getCharPos :: [[String]] -> Char -> Int -> Int-> Point
getCharPos tab@((x:xs):xs') c y x'
  | (head $ head $ head tab) == c = Point y x'
  | xs' == [] && xs == [] = Point 0 0
  | xs == [] = getCharPos xs' c (y+1) 0
  | otherwise = getCharPos (xs:xs') c y (x'+1)

--TODO !!!!
canInteractWith :: LevelMap -> Point -> Bool
canInteractWith lm p
 |isOnDisplayableMap lm p = elem (head $getCellAt (levelMap lm) p) ['K','m','*']
 |otherwise = False

canGoTrough :: LevelMap -> Point -> Bool
canGoTrough (LevelMap map1 _ _ _) p
 | elem (head (getCellAt map1 p) ) ['|','+','-','K','~','m'] = False
 | otherwise = True

willSay :: ConfigParser -> [[String]] -> Point -> String -> String
willSay rules map1 p' str =either (const str) id $ get rules "GAME" $ if length cell == 1 then cell else tail cell
  where
    cell = getCellAt map1 p'
