module Dialogue(
  Dialogue (..),
  getNewStartDialogue,
  getStrPart,
  getOptionsPart
  )
where

import Space
import UI.NCurses
import Data.List
import Data.Maybe

data Dialogue = Dialogue {str :: String,
  charpos :: Curses Int}

getNewStartDialogue :: String -> Int -> Direction -> Point -> Int
getNewStartDialogue str start dir (Point _ x)
  | dir == UP = if minusx >=0 && minusx < length str then minusx else start
  | dir == DOWN = if plusx >=0 && plusx < length str then plusx else start
  | otherwise = start
  where
    plusx = start+x
    minusx= start-x

getStrPart :: String -> String
getStrPart str = case ind of
  Just s ->  take (s-1) str
  Nothing -> str
  where
    ind = findIndex (=='|') str

getOptionsPart :: String -> [(String,String)]
getOptionsPart str = case ind of
  Just s -> zip( map head options) (map (\x -> if length x > 1 then x !!1 else head x) options)
  Nothing -> []
  where
    ind = findIndex (=='|') str
    options = map words $ explode (=='|') $ drop (fromJust ind) str

explode :: (Char -> Bool) -> String -> [String]
explode _ [] = []
explode f xs
    | null zs = [z]
    | null z  = explode f (tail zs)
    | True    = z : explode f (tail zs)
        where (z, zs) = break f xs
