module Keyboard
    (Keyboard (..),
    defaultKeyboard
    )
where

import UI.NCurses

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

defaultKeyboard :: Keyboard
defaultKeyboard = (Keyboard
 (EventCharacter 'z')
 (EventCharacter 's')
 (EventCharacter 'q')
 (EventCharacter 'd')
 (EventSpecialKey KeyUpArrow)
 (EventSpecialKey KeyDownArrow)
 (EventSpecialKey KeyLeftArrow)
 (EventSpecialKey KeyRightArrow)
 (EventCharacter 'l')
 (EventCharacter '\ESC')
 (EventCharacter 'h')
 )
