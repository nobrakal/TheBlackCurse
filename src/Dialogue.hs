module Dialogue(
  Dialogue (..)
  )
where

data Dialogue = Dialogue {str :: String,
  charpos :: Int} deriving (Show)
