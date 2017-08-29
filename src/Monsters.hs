module Monsters
    ( Monsters (..),
    removeDead,
    findActivated,
    getBeast,
    findActivatedInConfigParser
    ) where

import Data.List
import Data.ConfigFile
import Space
import Beast
import LevelMap

type Monsters = [Beast]

findActivated :: LevelMap -> ConfigParser -> Monsters
findActivated (LevelMap lm _) cp = findActivated' lm cp (findActivatedInConfigParser cp $ sections cp)

findActivated' :: Map -> ConfigParser -> [SectionSpec] -> Monsters
findActivated' _ _ [] = []
findActivated' lm cp (x:xs) = Beast (getStrPos lm x 0 0) DOWN (either (const 0) id $ get cp x "hp") (either (const 0) id $ get cp x "dammage") True : findActivated' lm cp xs

findActivatedInConfigParser :: ConfigParser -> [SectionSpec]-> [SectionSpec]
findActivatedInConfigParser _ [] = []
findActivatedInConfigParser cp (x:xs) = if either (const False) id $ get cp x "activated" then x : todo else todo
  where
    todo = findActivatedInConfigParser cp xs

-- TODO: replace by char
removeDead :: Map -> Monsters -> Map
removeDead m [] = m
removeDead m (Beast (Point y x) _ _ _ _:xs) = replaceByStr (removeDead m xs) y x "."

getBeast :: Monsters -> Point -> Maybe Beast
getBeast [] _ = Nothing
getBeast (x:xs) p
  | pos x == p = Just x
  | otherwise = getBeast xs p
