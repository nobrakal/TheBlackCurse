module Dialogue(
  Dialogue (..),
  getNewStartDialogue
  )
where

import Space
import UI.NCurses

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
