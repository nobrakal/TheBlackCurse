module Player
    ( Player (..)
    )
where

import LevelMap

data Player = Player {pos :: Point,
  pv :: Int}
