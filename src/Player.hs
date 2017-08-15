module Player
    ( Player (..),
    invertAtIndex,
    moveCAtPos,
    canGoTrough
    )
where

import LevelMap

data Player = Player {pos :: Point,
  pv :: Int}

-- WORK ONLY if length tab[y][x] >1
invertAtIndex :: Int -> Int -> [[[Char]]] -> [[[Char]]]
invertAtIndex y x tab=
  let (posy, posy',posx,posx') = truncateAt y x tab
      oldstr = (tab !! y) !! x
  in posy ++ [posx ++ [(([(head $ tail oldstr)] ++ [(head oldstr)]) ++ (tail $ tail oldstr))] ++ posx'] ++ posy'

-- Add a c at the pos
moveCAtPos :: Int -> Int -> Char -> [[String]] -> [[String]]
moveCAtPos y x c tab =
  let (posy, posy',posx,posx') = truncateAt y x tab
      oldstr = (tab !! y) !! x
  in posy ++ [posx ++ [c:oldstr] ++ posx'] ++ posy'

truncateAt :: Int -> Int -> [[a]] -> ([[a]], [[a]], [a], [a])
truncateAt y x tab = (take y tab,drop (y+1) tab, take x (tab !! y),drop (x+1) (tab !! y))

canGoTrough :: LevelMap -> Point -> Bool
canGoTrough (LevelMap map1 _ _ _) p
  | elem (head (getCellAt map1 p) ) ['|','+','-','K','~'] = False
  | otherwise = True
