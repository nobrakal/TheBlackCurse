module Draw
  (drawClearMsg,
  drawTab,
  makeBorders,
  getScreenSize,
  drawClearMsg',
  getWindowSize,
  appendMsg,
  msgWinHeight,
  calculateMainWinSize,
  calculateMsgWinSize,
  updateCamera
  )

where

import UI.NCurses
import Data.ConfigFile
import Data.List

import Space
import LevelMap
import GameTypes (Game (..))
import Beast (pos)

msgWinHeight :: ConfigParser -> Int
msgWinHeight x = either (const 5) read $ get x "GAME" "msgwinheight"

calculateMainWinSize :: ConfigParser -> Point -> Point
calculateMainWinSize c (Point y x ) = Point (y - msgWinHeight c -2) (x-2)

calculateMsgWinSize :: ConfigParser -> Point -> Point
calculateMsgWinSize c (Point _ x ) = Point (msgWinHeight c - 2) (x-2)

-- Move the camera (do not do any test)
updateCamera :: Window -> Game -> Curses()
updateCamera win (Game (LevelMap map1 p ) b _ rules _) = getScreenSize >>= \x -> drawTab win (calculateMainWinSize rules x) (getCurrentDisplay actualmap p (calculateMainWinSize rules x))
  where
    radius = either (const 0) read $ get rules "GAME" "radius"
    actualmap = if 0 < radius
      then getRadius map1 rules (pos b) radius
      else map1

-- Draw a message on the window if possible (clear all before)
drawClearMsg :: Window -> String -> Curses ()
drawClearMsg win str = updateWindow win $ windowSize>>= \arg -> drawClearMsg' (Point (fromIntegral (fst arg)) (fromIntegral (snd arg))) $ justifyRight (fromIntegral (snd arg)) str

-- Not made to be call directly
drawClearMsg' :: Point -> String -> Update ()
drawClearMsg' y_x_width str = do
  clear
  drawMsg y_x_width str

appendMsg :: Window -> String -> Curses ()
appendMsg win str = updateWindow win $ windowSize>>= \arg -> drawMsg (Point (fromIntegral (fst arg)) (fromIntegral (snd arg))) $ justifyRight (fromIntegral (snd arg)) str

drawMsg :: Point -> String -> Update ()
drawMsg y_x_width str = do
  let winW = y y_x_width * x y_x_width
  -- let str = show y_x_width ++ str'
  if length str <= winW
     then if length str == winW then drawString (init str) >> drawLineH (Just $ Glyph (last str) []) 1 else drawString str
     else drawString $ take (winW-1) str

-- Draw a tab of String
drawTab :: Window -> Point -> [[String]] -> Curses ()
drawTab win y_x_width tab = updateWindow win $ drawClearMsg' y_x_width $ concatMap (justifyRight (x y_x_width) . map head) tab

-- Draw borders of a rectangle starting at (pos_x,pos_y) with y rows and x columns on win
makeBorders :: Window -> Point -> Point -> Curses ()
makeBorders win (Point pos_y' pos_x') (Point y' x') = updateWindow win $ do
  moveCursor pos_y pos_x
  drawGlyph glyphCornerUL
  drawLineH (Just glyphLineH) $ x-2 -- Horizontal top line
  moveCursor (pos_y+1) pos_x
  drawLineV (Just glyphLineV) $ y-2 -- Vertical left line
  moveCursor (pos_y+y-1) pos_x
  drawGlyph glyphCornerLL
  moveCursor (pos_y+y-1) (pos_x+1)
  drawLineH (Just glyphLineH) $ x-2 -- Horizontal bottom line
  moveCursor (pos_y+y-1) (pos_x+x-1)
  drawLineH (Just glyphCornerLR) 1 -- drawGlyph move the cursor, we don't want it to go out of the window; drawlineH don't
  moveCursor pos_y (pos_x+x-1)
  drawGlyph glyphCornerUR
  moveCursor (pos_y+1) (pos_x+x-1)
  drawLineV (Just glyphLineV) $ y-2 -- Vertical Right line
  moveCursor pos_y pos_x -- Reset cursor pos
  where pos_x = toInteger pos_x'
        pos_y = toInteger pos_y'
        x = toInteger x'
        y = toInteger y'

getScreenSize :: Curses Point
getScreenSize = screenSize >>= \arg -> return (Point (fromIntegral (fst arg)) (fromIntegral (snd arg)))

getWindowSize :: Update Point
getWindowSize = windowSize >>= \arg -> return (Point (fromIntegral (fst arg)) (fromIntegral (snd arg)))
