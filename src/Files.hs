module Files
    (loadM,
    loadF,
    loadC
    ) where

import qualified Text.Read as R
import Data.Maybe
import Control.Exception
import Control.Monad
import System.IO.Error

import LevelMap
import Data.ConfigFile
import Space

-- Load the map file
loadM :: FilePath -> ConfigParser -> IO(Bool, LevelMap)
loadM mapPath fileRules= do
  e <- tryJust (guard . isDoesNotExistError) (readFile mapPath)
  let file = either (return ".") id e
  return (either (const False) (const True) e, loadMap file (either (const $ Point 0 0) ( fromMaybe (Point 0 0) . R.readMaybe) $ get fileRules "GAME" "currul")) --Init the map with screen size

-- Load the rule file
loadF :: FilePath -> IO ConfigParser
loadF rulesPath = do
  fr <- readfile emptyCP rulesPath
  return $  either (return emptyCP) id fr

-- Load the config file if provided
loadC :: FilePath -> IO ConfigParser
loadC fp = do
  cp <- readfile emptyCP fp
  return $ either (return emptyCP) id cp
