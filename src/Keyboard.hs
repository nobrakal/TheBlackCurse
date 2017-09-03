{-# LANGUAGE TemplateHaskell #-}

module Keyboard
    (Keyboard (..),
    defaultKeyboard,
    buildKeyboard',
    getEvents,
    getC,
    getDir
    )
where

import UI.NCurses
import Data.ConfigFile
import Data.ConfigFile.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import LevelMap
import Space

data Keyboard = Kbd {
  up :: Event,
  down :: Event,
  left :: Event,
  right :: Event,
  cUp :: Event,
  cDown :: Event,
  cLeft :: Event,
  cRight :: Event,
  action :: Event,
  load :: Event,
  save :: Event,
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
  cp <- set cp "KEYBOARD" "exit" "ESC"
  cp <- set cp "KEYBOARD" "help" "h"
  cp <- set cp "KEYBOARD" "one" "1"
  cp <- set cp "KEYBOARD" "two" "2"
  cp <- set cp "KEYBOARD" "three" "3"
  cp <- set cp "KEYBOARD" "four" "4"
  set cp "KEYBOARD" "five" "5"

-- Produce a function withe name that build a keyboard where evry record is set by func. Func will receive record names
buildKeyboard' :: String -> (Exp -> [String] -> [Q Exp]) -> Q [Dec]
buildKeyboard' name func = do
  cp <- newName "cp"
  fnName <- newName name
  let kbd =  mkName "Kbd"
  ncp <- varE cp
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify ''Keyboard
  let names = map (\(name,_,_) -> name) fields
  let varPat = [varP cp]
  evs <- sequenceQ $ func ncp $ map (drop 9 . show) names --TODO: Better than drop
  let fexp = zipWith (curry  return) names evs
  let final = recConE kbd fexp
  sequenceQ [sigD fnName (appT (appT arrowT $ conT ''ConfigParser) (conT ''Keyboard)),funD fnName [clause varPat (normalB final) []]]

-- Take a ConfigParser and build with it a keyboard
getEvents :: Exp -> [String] -> [Q Exp]
getEvents _ [] = []
getEvents cp (x:xs) = [| getC $(return cp) "KEYBOARD" x |] : getEvents cp xs

getC :: ConfigParser -> String -> String -> Event
getC c section str = either (return $ EventUnknown 0) getE $ get c section str

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
