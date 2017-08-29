module Dialogue(
  Dialogue (..),
  getNewStartDialogue,
  getStrPart,
  getOptionsPart,
  newDialogue,
  showOptions,
  setOrUnsetLastoption,
  showDialogue,
  isEnded
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
  options :: Maybe [(OptionSpec,String)],
  -- Maybe the last item specified in the options list
  lastoption :: Maybe OptionSpec}

showDialogue :: Dialogue -> Int -> Direction -> Point -> String
showDialogue (Dialogue str _ _ options _) y dir p =  drop (getNewStartDialogue str' y dir p) str' ++ "\n" ++ showOptions options
  where
    str' = getStrPart str

-- Create a new dialogue with the specified ConfigParser, String and SectionSpec. If the Bool is set to True, it will check the lastoption, else no.
newDialogue :: ConfigParser -> OptionSpec -> SectionSpec -> Bool -> Dialogue
newDialogue cp op section realynew = Dialogue str' (return 0) section (if null optionspart || null iOptionpart then Nothing else Just iOptionpart ) (if null optionspart || null (snd $last optionspart)  then Nothing else Just $ fst $ last optionspart)
  where
    str = either (const "ERROR IN DIALGOUE") id $ get cp section op
    str' = if has_option cp section "lastoption" && realynew then either (const "error") id $ get cp section last' else str
    last' = either (const "") id $ get cp section "lastoption"
    optionspart = getOptionsPart str'
    iOptionpart = init optionspart

isEnded :: ConfigParser -> SectionSpec -> Bool
isEnded cp s
  | lastop == "END" = True
  | otherwise = False
  where
    lastop = either (const "") id $ get cp s "lastoption"

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

getOptionsPart :: String -> [(OptionSpec,String)]
getOptionsPart str = case ind of
  Just s -> zip (map head options) (map (\x -> if length x > 1 then concatMap (++" ") $ tail x else head x) options)
  Nothing -> []
  where
    ind = elemIndex '|' str
    options = map words $ explode (=='|') $ drop (fromJust ind) str

showOptions :: Maybe [(String,String)] -> String
showOptions (Just s) = showOptions' s 1
showOptions Nothing = ""

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

setOrUnsetLastoption :: ConfigParser -> SectionSpec -> Maybe OptionSpec -> ConfigParser
setOrUnsetLastoption rules' section lastoption = either (const rules') id $ maybe (remove_option rules' section "lastoption") (set rules' section "lastoption") lastoption
