module Player
    ( Player (..)
    )
where

import LevelMap

data Player = Player {charpos :: Point,
  pv :: Int}
