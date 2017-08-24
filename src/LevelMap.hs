module LevelMap (
  LevelMap (..),
  Map (..),
  loadMap,
  toStr,
  isOnDisplayableMap,
  getCurrentDisplay,
  getCellAt,
  getCharPos,
  canInteractWith,
  canGoTrough,
  willDo,
  getRadius)
where

import Space
import Data.ConfigFile

type Map = [[String]]

data LevelMap = LevelMap {levelMap :: Map,
  currul :: Point -- Current upper left corner of the displayed area
}

loadMap :: String -> Point -> LevelMap
loadMap file currul = do
  let file_map = map (++ ["\n"])$ map words $lines file -- ++ [[" "]]
  LevelMap file_map currul

toStr :: Map -> String
toStr = concatMap concat

getmaxLength :: [[a]] -> Int
getmaxLength = foldr (max . length) 0

-- Return true if the point is on the map
isOnDisplayableMap :: LevelMap -> Point -> Bool
isOnDisplayableMap (LevelMap tab _) (Point y x) = (x>=0) && (y>=0) && (x < xw) && (y < yw)
  where
    yw = length tab
    xw = (-1) + length (head tab)

-- Reduce if possible the map to a map of (height,width) starting at (starty,startx)
getCurrentDisplay :: [[String]] -> Point -> Point -> Map
getCurrentDisplay tab (Point starty startx) (Point height width) = take height $ map (take width) $ drop starty $ map (drop startx) tab

getCellAt :: [[a]] -> Point -> a
getCellAt tab (Point y x) = (tab !! y) !! x

-- Find the "@" on the map
getCharPos :: Map -> Char -> Int -> Int-> Point
getCharPos tab@((x:xs):xs') c y x'
  | head ( head $ head tab )== c = Point y x'
  | null xs' && null xs = Point (-1) (-1)
  | null xs = getCharPos xs' c (y+1) 0
  | otherwise = getCharPos (xs:xs') c y (x'+1)

{- Interract things -}

canInteractWith :: LevelMap -> Point -> ConfigParser -> String -> Bool
canInteractWith lm p cp todo
 |isOnDisplayableMap lm p = has_option cp (getCellAt (levelMap lm) p) todo
 |otherwise = False

canGoTrough :: LevelMap -> Point -> ConfigParser -> Bool
canGoTrough (LevelMap map1 _) p cp
 | elem (head (getCellAt map1 p) ) $ either (const "") id $ get cp "GAME" "cannotgothrough" = False
 | otherwise = True

willDo :: ConfigParser -> [[String]] -> Point -> String -> String -> String
willDo rules map1 p' sec str =either (const str) id $ get rules cell sec
  where
    cell = getCellAt map1 p'

{- Radius things -}

getRadius :: [[String]] -> ConfigParser -> Point -> Int -> [[String]]
getRadius map1 cf start radius_w = applyMask map1 emptyMap $ getRadiusFromPoint start radius_w
  where
    emptyMap = replicate (length map1) ((++ ["\n"]) (replicate (length $ head map1) " "))

applyMask :: [[String]] -> [[String]] -> [Point] -> [[String]]
applyMask tab emptyMap [] = emptyMap
applyMask tab emptyMap (p@(Point y x):xs) = if isOnDisplayableMap (LevelMap tab (Point 0 0)) p
  then applyMask tab (replaceByStr emptyMap y x (getCellAt tab p)) xs
  else applyMask tab emptyMap xs

replaceByStr :: [[String]] -> Int -> Int -> String -> [[String]]
replaceByStr tab y x str = a ++ ((a' ++ (str:(if b' /= [] then tail b' else b'))):(if b /= [] then tail b else b))
  where
    (a,b@(x':xs)) = splitAt y tab
    (a',b') = splitAt x x'
