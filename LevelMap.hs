module LevelMap (LevelMap (..))
where

data LevelMap = LevelMap {levelMap :: [[Char]],
  curr_Displayed :: [[Char]],
  playerStartPos :: (Int,Int)}

loadMap :: FilePath -> (Int,Int) -> IO LevelMap
loadMap path y_x_width = do
  file <- (readFile path)
  let file_map = lines file
  return (LevelMap file_map (getCurrentDisplay file_map y_x_width) (0,0))

-- Reduce if possible the map to a map of (height,width)
getCurrentDisplay :: [[Char]] -> (Int,Int) -> [[Char]]
getCurrentDisplay tab (height,width) = take height $ map (take width) tab
