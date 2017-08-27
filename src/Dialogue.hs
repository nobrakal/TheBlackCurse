module Dialogue(
  Dialogue (..),
  getNewStartDialogue,
  getStrPart,
  getOptionsPart,
  newDialogue,
  showOptions
  )
where

import Space
import UI.NCurses
import Data.List
import Data.Maybe
import Data.ConfigFile

data Dialogue = Dialogue {str :: String,
  charpos :: Curses Int,
  section :: SectionSpec,
  options :: [(OptionSpec,String)]}

newDialogue :: ConfigParser -> String -> SectionSpec -> Dialogue
newDialogue cp str section = Dialogue (str') (return 0) section (getOptionsPart str')
  where
    str' = if has_option cp section "lastoption" then either (const "error") id $ get cp section last' else str
    last' = either (const "") id $ get cp section "lastoption"

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
    ind = elemIndex '|' str

getOptionsPart :: String -> [(String,String)]
getOptionsPart str = case ind of
  Just s -> zip( map head options) (map (\x -> if length x > 1 then x !!1 else head x) options)
  Nothing -> []
  where
    ind = elemIndex '|' str
    options = map words $ explode (=='|') $ drop (fromJust ind) str

showOptions :: [(String,String)] -> String
showOptions s = showOptions' s 1

showOptions' :: [(String,String)] -> Int -> String
showOptions' [] _ = ""
showOptions' ((_,str):xs) x = "(" ++ show x ++  "): " ++ str ++ "\n" ++ showOptions' xs (x+1)

explode :: (Char -> Bool) -> String -> [String]
explode _ [] = []
explode f xs
    | null zs = [z]
    | null z  = explode f (tail zs)
    | otherwise = z : explode f (tail zs)
        where (z, zs) = break f xs
