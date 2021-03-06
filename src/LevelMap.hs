module LevelMap (
  LevelMap (..),
  Map (..),
  loadMap,
  toStr,
  isOnDisplayableMap,
  getCurrentDisplay,
  getCellAt,
  getCharPos,
  getStrPos,
  canInteractWith,
  canGoTrough,
  getRadius,
  replaceByStr,
  justifyRight,
  findWithPrefix)
where

import Space
import Data.ConfigFile
import Data.List

import Data.Maybe (isJust)

type Map = [[String]]

data LevelMap = LevelMap {levelMap :: Map,
  currul :: Point -- Current upper left corner of the displayed area
}

loadMap :: String -> Point -> LevelMap
loadMap file currul = LevelMap (map words $ lines file) currul

toStr :: Map -> String
toStr x= unlines $ map unwords x

getmaxLength :: [[a]] -> Int
getmaxLength = foldr (max . length) 0

-- Return true if the point is on the map
isOnDisplayableMap :: LevelMap -> Point -> Bool
isOnDisplayableMap (LevelMap tab _) (Point y x) = (x>=0) && (y>=0) && (x < xw) && (y < yw)
  where
    yw = length tab
    xw = (-1) + length (head tab)

justifyRight :: Int -> String -> String
justifyRight k t
    | len >= k  = t
    | otherwise = t ++ replicate (k-len) ' '
  where len = length t

-- Reduce if possible the map to a map of (height,width) starting at (starty,startx)
getCurrentDisplay :: [[String]] -> Point -> Point -> Map
getCurrentDisplay tab (Point starty startx) (Point height width) = take height $ map (take width) $ drop starty $ map (drop startx) tab

getCellAt :: [[a]] -> Point -> a
getCellAt tab (Point y x) = (tab !! y) !! x

-- Find the "@" on the map
getCharPos :: Map -> Char -> Int -> Int-> Point
getCharPos tab c = getStrPos tab [c]

getStrPos :: Map -> String -> Int -> Int-> Point
getStrPos tab@((x:xs):xs') s y x'
  | isPrefixOf s $ head (head tab) = Point y x'
  | null xs' && null xs = Point (-1) (-1)
  | null xs = getStrPos xs' s (y+1) 0
  | otherwise = getStrPos (xs:xs') s y (x'+1)

-- Find in config parser with prefix
findWithPrefix :: ConfigParser -> OptionSpec -> SectionSpec -> String -> String
findWithPrefix cp option section con = maybe con ( \x -> either (const con) id $ get cp x option) $ findPrefix cp option section

findPrefix :: ConfigParser -> OptionSpec -> SectionSpec -> Maybe SectionSpec
findPrefix _ _ [] = Nothing
findPrefix cp option str = if has_section cp str && has_option cp str option then Just str else findPrefix cp option $ init str

{- Interract things -}

canInteractWith :: LevelMap -> Point -> ConfigParser -> String -> Bool
canInteractWith lm p cp todo
 |isOnDisplayableMap lm p = isJust $ findPrefix cp todo $ getCellAt (levelMap lm) p
 |otherwise = False

canGoTrough :: LevelMap -> Point -> ConfigParser -> Bool
canGoTrough (LevelMap map1 _) p cp
 | elem (head (getCellAt map1 p) ) $ either (const "") id $ get cp "GAME" "cannotgothrough" = False
 | otherwise = True

{- Radius things -}
 -- TODO Shadow part
getRadius :: Map -> ConfigParser -> Point -> Int -> Map
getRadius map1 cf start radius_w = applyMask map1 emptyMap $ onlyExistingPoint map1 $ getRadiusFromPoint start radius_w
  where
    emptyMap = replicate (length map1) (replicate (length $ head map1) " ")

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

{- Detect things between the player and the rest of the world -}
makeShadow' :: ConfigParser -> Map -> Point -> [Point] -> [Point]
makeShadow' cf tab start t = makeShadow cf tab start (if null t then Point 0 0 else head t) t

makeShadow :: ConfigParser -> Map -> Point -> Point -> [Point] -> [Point]
makeShadow _ _ _ _ [] = []
makeShadow cf tab start curr t@(x:xs)
  | newpos == start = x : makeShadow cf tab start (nullOrP xs) xs
  | otherwise = if head (getCellAt tab newpos) `elem` cannotSee then makeShadow cf tab start (nullOrP xs) xs else makeShadow cf tab start newpos t
  where
    newpos = curr + signum (start - curr)
    cannotSee = either (const "") id $ get cf "GAME" "cannotseethrough"
    nullOrP xs = if null xs then Point 0 0 else head xs

onlyExistingPoint ::  Map -> [Point] -> [Point]
onlyExistingPoint _ [] = []
onlyExistingPoint tab (x:xs)
  | isOnDisplayableMap (LevelMap tab (Point 0 0)) x = x : onlyExistingPoint tab xs
  | otherwise = onlyExistingPoint tab xs
