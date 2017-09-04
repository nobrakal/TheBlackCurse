module GameTypes
    ( Common (..),
    Game (..),
    State (..),
    Status (..)
    ) where

import UI.NCurses
import Data.ConfigFile

import Space
import Keyboard
import LevelMap
import Beast
import Dialogue

data Common = Common {
  stdscr :: Window,
  mainWin :: Window,
  msgWin :: Window,
  mapPath :: FilePath,
  rulesPath :: FilePath,
  confPath :: Maybe FilePath,
  keyboard :: Keyboard
}

data Game = Game {
  m :: LevelMap,
  player :: Beast,
  monsters :: Monsters,
  rules :: ConfigParser,
  dialogue :: Dialogue
}

data State = State {
  common :: Common,
  game :: Game,
  status :: Status,
  todo :: Curses ()
}

data Status = MainGame | InDialogue | Dead | Quit | Load | Action deriving (Show, Eq) -- Action is used when we have to determine the status
