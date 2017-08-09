module LevelMap (LevelMap (..),
  loadMap)
where

data LevelMap = LevelMap {levelMap :: [[Char]],
  c_y_x :: (Integer,Integer) -- Current left corner of the displayed area
}

loadMap :: FilePath -> IO LevelMap
loadMap path = do
  file <- (readFile path)
  let file_map = map (++ "\n") $lines file
  return (LevelMap file_map (0,0))

getmaxLength :: [[a]] -> Int
getmaxLength [] = 0
getmaxLength (x:xs) = max (length x) (getmaxLength xs)
