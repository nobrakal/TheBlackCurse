module LevelMap (LevelMap (..),
  loadMap)
where

data LevelMap = LevelMap {levelMap :: [[Char]],
  curr_displayed_y_x :: (Integer,Integer)}

loadMap :: FilePath -> IO LevelMap
loadMap path = do
  file <- (readFile path)
  let file_map = lines file
  return (LevelMap file_map (0,0))
