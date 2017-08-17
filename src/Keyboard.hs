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
  exit :: Event,
  help :: Event
} deriving (Show)

defaultKeyboard :: ConfigParser --default kbd
defaultKeyboard = either (return emptyCP) id $ do
  let cp = emptyCP
  cp <- add_section cp "KEYBOARD"
  cp <- set cp "KEYBOARD" "up" "z"
  cp <- set cp "KEYBOARD" "down" "s"
  cp <- set cp "KEYBOARD" "lef" "q"
  cp <- set cp "KEYBOARD" "right" "d"
  cp <- set cp "KEYBOARD" "cUp" "KeyUpArrow"
  cp <- set cp "KEYBOARD" "cDown" "KeyDownArrow"
  cp <- set cp "KEYBOARD" "cLef" "KeyLeftArrow"
  cp <- set cp "KEYBOARD" "cRight" "KeyRightArrow"
  cp <- set cp "KEYBOARD" "action" "e"
  cp <- set cp "KEYBOARD" "load" "l"
  cp <- set cp "KEYBOARD" "quit" "ESC"
  cp <- set cp "KEYBOARD" "help" "h"
  return cp

loadKeyboard :: ConfigParser -> Keyboard
loadKeyboard c = (Keyboard
  (getC c "KEYBOARD" "up")
  (getC c "KEYBOARD" "down")
  (getC c "KEYBOARD" "left")
  (getC c "KEYBOARD" "right")
  (getC c "KEYBOARD" "cUp")
  (getC c "KEYBOARD" "cDown")
  (getC c "KEYBOARD" "cLeft")
  (getC c "KEYBOARD" "cRight")
  (getC c "KEYBOARD" "action")
  (getC c "KEYBOARD" "load")
  (getC c "KEYBOARD" "quit")
  (getC c "KEYBOARD" "help")
  )

getC :: ConfigParser -> String -> String -> Event
getC c section str =either (return $ EventCharacter 'n') (getE) $ (get c section str)

getE :: String -> Event
getE s
  | length s == 1 = EventCharacter $ head s
  | s == "ESC" = EventCharacter '\ESC'
  | s == "KeyUpArrow" = EventSpecialKey KeyUpArrow
  | s == "KeyDownArrow" = EventSpecialKey KeyDownArrow
  | s == "KeyLeftArrow" = EventSpecialKey KeyLeftArrow
  | s == "KeyRightArrow" = EventSpecialKey KeyRightArrow
  |otherwise = EventUnknown 0

getDir :: Keyboard -> Event -> Point
getDir k s
  | s== up k || s == cUp k  = Point (-1) 0
  | s== down k || s == cDown k  = Point 1 0
  | s== left k || s == cLeft k  = Point 0 (-1)
  | s== right k || s == cRight k = Point 0 1
  | otherwise = Point 0 0
