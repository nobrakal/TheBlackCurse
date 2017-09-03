module Monsters
    ( Monsters (..),
    removeDead,
    findActivated,
    findMonsters,
    getBeast
    ) where

import Data.List
import Data.ConfigFile
import Space
import Beast
import LevelMap

type Monsters = [Beast]

findMonsters :: LevelMap -> ConfigParser -> Monsters
findMonsters (LevelMap lm _) cp = findMonsters' lm cp $ findMonstersInCp cp $ sections cp

findMonsters' :: Map -> ConfigParser -> [SectionSpec] -> Monsters
findMonsters' _ _ [] = []
findMonsters' lm cp (x:xs) = Beast (getStrPos lm x 0 0) DOWN (either (const 0) id $ get cp x "hp") (either (const 0) id $ get cp x "dammage") (either (const 0) id $ get cp x "activated") (either (const "no name") id $ get cp x "name") : findMonsters' lm cp xs

findMonstersInCp :: ConfigParser -> [SectionSpec]-> [SectionSpec]
findMonstersInCp _ [] = []
findMonstersInCp cp (x:xs) = if has_option cp x "hp" && x /= "PLAYER" then x : findMonstersInCp cp xs else findMonstersInCp cp xs

findActivated :: Point -> ConfigParser -> Monsters -> Monsters
findActivated _ _ [] = []
findActivated charpos cp (x:xs) = if dist (pos x) charpos <= activated x then x : todo else todo
  where
    todo = findActivated charpos cp xs

removeDead :: Map -> Monsters -> Map
removeDead m [] = m
removeDead m (Beast p@(Point y x) _ _ _ _ _:xs) = replaceByStr (removeDead m xs) y x [last $ getCellAt m p]

getBeast :: Monsters -> Point -> Maybe Beast
getBeast [] _ = Nothing
getBeast (x:xs) p
  | pos x == p = Just x
  | otherwise = getBeast xs p
