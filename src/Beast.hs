module Beast
    ( Beast (..),
    removeFirstCharAt,
    moveCAtPos
    )
where

import Space
import LevelMap

data Beast = Beast {pos :: Point,
  look :: Direction,
  pv :: Int}

-- WORK ONLY if length tab[y][x] >1
{- invertAtIndex :: Int -> Int -> Map -> Map
invertAtIndex y x tab=
  let (posy, posy',posx,posx') = truncateAt y x tab
      oldstr = (tab !! y) !! x
  in posy ++ [posx ++ [([head $ tail oldstr] ++ [head oldstr]) ++ tail (tail oldstr)] ++ posx'] ++ posy' -}

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
