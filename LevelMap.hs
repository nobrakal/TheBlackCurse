module LevelMap (LevelMap (..),
  loadMap,
  getCurrentDisplay)
where

data LevelMap = LevelMap {levelMap :: [[Char]],
  curr_displayed_y_x :: (Integer,Integer)}

loadMap :: FilePath -> IO LevelMap
loadMap path = do
  file <- (readFile path)
  let file_map = lines file
  return (LevelMap file_map (0,0))

-- Reduce if possible the map to a map of (height,width) starting at (starty,startx)
getCurrentDisplay :: LevelMap -> (Integer,Integer) -> (Integer,Integer) -> [[Char]]
getCurrentDisplay tab (starty,startx) (height,width)  = take (fromIntegral height) $ map (take (fromIntegral width)) $ drop (fromIntegral starty) $ map (drop (fromIntegral startx)) $ levelMap tab
