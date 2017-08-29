module Beast
    ( Beast (..),
    removeFirstCharAt,
    moveCAtPos,
    findActivated
    )
where

import Data.ConfigFile

import Space
import LevelMap

data Beast = Beast {pos :: Point,
  look :: Direction,
  hp :: Int,
  dammage :: Int,
  activated :: Bool
}

removeFirstCharAt :: Int -> Int -> Map -> Map
removeFirstCharAt y x tab=
  let (posy, posy',posx,posx') = truncateAt y x tab
      oldstr = (tab !! y) !! x
  in posy ++ [posx ++ [tail oldstr] ++ posx'] ++ posy'

-- Add a c at the pos
moveCAtPos :: Int -> Int -> Char -> Map -> Map
moveCAtPos y x c tab =
  let (posy, posy',posx,posx') = truncateAt y x tab
      oldstr = (tab !! y) !! x
  in posy ++ [posx ++ [c:oldstr] ++ posx'] ++ posy'

truncateAt :: Int -> Int -> [[a]] -> ([[a]], [[a]], [a], [a])
truncateAt y x tab = (take y tab,drop (y+1) tab, take x (tab !! y),drop (x+1) (tab !! y))

findActivated :: LevelMap -> ConfigParser -> [Beast]
findActivated (LevelMap lm _) cp = findActivated' lm cp (findActivatedInConfigParser cp $ sections cp)

findActivated' :: Map -> ConfigParser -> [SectionSpec] -> [Beast]
findActivated' _ _ [] = []
findActivated' lm cp (x:xs) = Beast (getStrPos lm x 0 0) DOWN (either (const 0) id $ get cp x "hp") (either (const 0) id $ get cp x "dammage") True : findActivated' lm cp xs

findActivatedInConfigParser :: ConfigParser -> [SectionSpec]-> [SectionSpec]
findActivatedInConfigParser _ [] = []
findActivatedInConfigParser cp (x:xs) = (if either (const False) id $ get cp x "activated" then x else []) : findActivatedInConfigParser cp xs
