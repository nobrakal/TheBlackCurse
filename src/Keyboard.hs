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
  exit :: Event
}

defaultKeyboard :: Keyboard
defaultKeyboard = (Keyboard (EventSpecialKey KeyUpArrow)
 (EventSpecialKey KeyDownArrow)
 (EventSpecialKey KeyLeftArrow)
 (EventSpecialKey KeyRightArrow)
 (EventCharacter 'z')
 (EventCharacter 's')
 (EventCharacter 'q')
 (EventCharacter 'd')
 (EventCharacter 'l')
 (EventCharacter '\ESC')
 )
