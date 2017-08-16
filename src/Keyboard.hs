module Keyboard
    (Keyboard (..),
    defaultKeyboard,
    loadKeyboard
    )
where

import UI.NCurses
import Data.ConfigFile

data Keyboard = Keyboard {
  up :: Event,
  down :: Event,
  left :: Event,
  right :: Event,
  cUp :: Event,
  cDown :: Event,
  cLeft :: Event,
  cRight :: Event,
  loadM :: Event,
  exit :: Event,
  help :: Event
} deriving (Show)

defaultKeyboard :: ConfigParser --default kbd
defaultKeyboard = either (return emptyCP) id $ do
  let cp = emptyCP
  cp <- add_section cp "KEYBOARD"
  cp <- set cp "KEYBOARD" "up" "'z'"
  set cp "KEYBOARD" "down" "'s'"
  set cp "KEYBOARD" "lef" "'q'"
  set cp "KEYBOARD" "right" "'d'"
  set cp "KEYBOARD" "load" "'l'"
  set cp "KEYBOARD" "quit" "'\ESC'"
  set cp "KEYBOARD" "help" "'h'"
  return cp

-- TODO get Arrow key
loadKeyboard :: ConfigParser -> Keyboard
loadKeyboard c = (Keyboard
  (EventCharacter $ getC c "KEYBOARD" "up")
  (EventCharacter $ getC c "KEYBOARD" "down")
  (EventCharacter $ getC c "KEYBOARD" "left")
  (EventCharacter $ getC c "KEYBOARD" "right")
  (EventSpecialKey KeyUpArrow)
  (EventSpecialKey KeyDownArrow)
  (EventSpecialKey KeyLeftArrow)
  (EventSpecialKey KeyRightArrow)
  (EventCharacter $ getC c "KEYBOARD" "load")
  (EventCharacter $ getC c "KEYBOARD" "quit")
  (EventCharacter $ getC c "KEYBOARD" "help")
  )

getC :: ConfigParser -> String -> String -> Char
getC c section str =either (return 'n') id $ get c section str
