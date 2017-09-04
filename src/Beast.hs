module Beast
    ( Beast (..),
    Monsters,
    removeFirstCharAt,
    moveCAtPos,
    getStatus
    )
where

import Space
import LevelMap

type Monsters = [Beast]

data Beast = Beast {pos :: Point,
  look :: Direction,
  hp :: Int,
  dammage :: Int,
  activated :: Int, -- radius
  name :: String
} deriving (Show,Eq)

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

getStatus :: Beast -> String
getStatus (Beast _ _ h d _ n) = n ++ " has " ++ show h ++ " hp and will make " ++ show d ++ " dammages to h{is,er} opponents"
