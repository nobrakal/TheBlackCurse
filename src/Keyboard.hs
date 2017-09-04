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
  view :: Event,
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
defaultKeyboard = foldl (\x (y1,y2)-> either (const emptyCP) id $ set x "KEYBOARD" y1 y2) cp defaults
  where
    cp = either (const emptyCP) id $ add_section emptyCP "KEYBOARD"
    defaults = [("up","z" ),("down","s"),("left","q"),("right","d")  ,("cUp","KeyUpArrow"), ("cDown", "KeyDownArrow"),("cLeft", "KeyLeftArrow"),("cRight", "KeyRightArrow"),("action", "e"),("view", "v"),("load", "l"),("save", "m"),("exit", "ESC"),("help", "h"),("one", "1"),("two", "2"),("three", "3"),("four", "4"),("five", "5")]

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
