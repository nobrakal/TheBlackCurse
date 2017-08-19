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
  currul :: Point -- Current upper left corner of the displayed area
}

loadMap :: String -> Point -> LevelMap
loadMap file currul = do
  let file_map = (map (++ ["\n"])$ map words $lines file) -- ++ [[" "]]
  LevelMap file_map currul

getmaxLength :: [[a]] -> Int
getmaxLength [] = 0
getmaxLength (x:xs) = max (length x) (getmaxLength xs)

-- Return true if the point is on the map
isOnDisplayableMap :: LevelMap -> Point -> Bool
isOnDisplayableMap (LevelMap tab _) (Point y x) = (x>=0) && (y>=0) && (x < xw) && (y < yw)
  where
    yw = length tab
    xw = (-1) + length (head tab)

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

canInteractWith :: LevelMap -> Point -> ConfigParser -> Bool
canInteractWith lm p cp
 |isOnDisplayableMap lm p = elem (head $getCellAt (levelMap lm) p) $ map head $ sections cp
 |otherwise = False

canGoTrough :: LevelMap -> Point -> ConfigParser -> Bool
canGoTrough (LevelMap map1 _) p cp
 | elem (head (getCellAt map1 p) ) $ either (const "") id $ get cp "GAME" "cannotgothrough" = False
 | otherwise = True

willSay :: ConfigParser -> [[String]] -> Point -> String -> String
willSay rules map1 p' str =either (const str) id $ get rules cell "tosay"
  where
    cell = getCellAt map1 p'
