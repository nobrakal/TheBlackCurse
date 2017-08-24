module Keyboard
    (Keyboard (..),
    defaultKeyboard,
    loadKeyboard,
    getDir
    )
where

import UI.NCurses
import Data.ConfigFile

import LevelMap
import Space

data Keyboard = Keyboard {
  up :: Event,
  down :: Event,
  left :: Event,
  right :: Event,
  cUp :: Event,
  cDown :: Event,
  cLeft :: Event,
  cRight :: Event,
  action :: Event,
  loadM :: Event,
  saveM :: Event,
  exit :: Event,
  help :: Event,
  one :: Event,
  two :: Event,
  three :: Event,
  four :: Event,
  five :: Event
} deriving (Show)

defaultKeyboard :: ConfigParser --default kbd
defaultKeyboard = either (return emptyCP) id $ do
  let cp = emptyCP
  cp <- add_section cp "KEYBOARD"
  cp <- set cp "KEYBOARD" "up" "z"
  cp <- set cp "KEYBOARD" "down" "s"
  cp <- set cp "KEYBOARD" "left" "q"
  cp <- set cp "KEYBOARD" "right" "d"
  cp <- set cp "KEYBOARD" "cUp" "KeyUpArrow"
  cp <- set cp "KEYBOARD" "cDown" "KeyDownArrow"
  cp <- set cp "KEYBOARD" "cLeft" "KeyLeftArrow"
  cp <- set cp "KEYBOARD" "cRight" "KeyRightArrow"
  cp <- set cp "KEYBOARD" "action" "e"
  cp <- set cp "KEYBOARD" "load" "l"
  cp <- set cp "KEYBOARD" "save" "m"
  cp <- set cp "KEYBOARD" "quit" "ESC"
  cp <- set cp "KEYBOARD" "help" "h"
  cp <- set cp "KEYBOARD" "one" "1"
  cp <- set cp "KEYBOARD" "two" "2"
  cp <- set cp "KEYBOARD" "three" "3"
  cp <- set cp "KEYBOARD" "four" "4"
  set cp "KEYBOARD" "five" "5"

loadKeyboard :: ConfigParser -> Keyboard
loadKeyboard c = Keyboard
  (get "up")
  (get "down")
  (get "left")
  (get "right")
  (get "cUp")
  (get "cDown")
  (get "cLeft")
  (get "cRight")
  (get "action")
  (get "load")
  (get "save")
  (get "quit")
  (get "help")
  (get "one")
  (get "two")
  (get "three")
  (get "four")
  (get "five")
  where
    get = getC c "KEYBOARD"

getC :: ConfigParser -> String -> String -> Event
getC c section str = either (return $ EventCharacter 'n') getE $ get c section str

getE :: String -> Event
getE s
  | length s == 1 = EventCharacter $ head s
  | s == "ESC" = EventCharacter '\ESC'
  | s == "KeyUpArrow" = EventSpecialKey KeyUpArrow
  | s == "KeyDownArrow" = EventSpecialKey KeyDownArrow
  | s == "KeyLeftArrow" = EventSpecialKey KeyLeftArrow
  | s == "KeyRightArrow" = EventSpecialKey KeyRightArrow
  |otherwise = EventUnknown 0

getDir :: Keyboard -> Event -> Direction
getDir k s
  | s== up k || s == cUp k  = UP
  | s== down k || s == cDown k  = DOWN
  | s== left k || s == cLeft k  = LEFT
  | s== right k || s == cRight k = RIGHT
  | otherwise = NULL
