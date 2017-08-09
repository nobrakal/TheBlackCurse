import UI.NCurses
import System.Exit
import Data.List

import LevelMap

-- NOTE: Curses is a wrapper for IO

msgWin_height :: Integer
msgWin_height = 5

data Game = Game {
  stdscr :: Window,
  mainWin :: Window,
  msgWin :: Window,
  m :: LevelMap
}

main :: IO ()
main = do
  map1 <- loadMap "./maps/map1.txt"

  runCurses $ do --Start
    setEcho False -- Disable echo
    --setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    y_x_width <- screenSize

    updateBorders stdscr y_x_width

    let msdim = calculateMsgWinSize y_x_width
    let mwdim = calculateMainWinSize y_x_width
    msgWin <- newWindow (fst msdim) (snd msdim) 1 1 -- msg window
    mainWin <- newWindow (fst mwdim) (snd mwdim) (msgWin_height+1) 1 -- bottom window

    drawTab mainWin $ getCurrentDisplay map1 (0,0) (y_x_width)
    -- updateWindow stdscr $  moveCursor (fst y_x_width-1) (snd y_x_width-1)
    render

    mainLoop (Game stdscr mainWin msgWin map1) -- Run mainLoop

mainLoop :: Game -> Curses ()
mainLoop (Game stdscr mainWin msgWin map1) = do
  inp <- getEvent stdscr Nothing

  if (inp == (Just (EventCharacter 'q'))) || (inp == (Just (EventCharacter 'Q'))) || (inp == (Just (EventCharacter '\ESC'))) then
   return ()
  else do
    useInput (Game stdscr mainWin msgWin map1)  inp
    render
    mainLoop (Game stdscr mainWin msgWin map1)


useInput :: Game -> Maybe Event -> Curses ()
useInput game (Just (EventSpecialKey s))
  | (s==KeyUpArrow) || (s==KeyDownArrow) || (s==KeyLeftArrow) || (s==KeyRightArrow) = drawClearMsg (msgWin game) "A direction was pressed" -- moveCharacter s
useInput game (Just (EventUnknown s)) = drawClearMsg (msgWin game) $ "ERROR WITH EVENT" ++ show s -- ERROR
useInput (Game stdscr mainWin msgWin map1) (Just (EventResized)) = do -- Resized
  y_x_width <- screenSize
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  updateWindow msgWin $ resizeWindow (fst msdim) (snd msdim)
  updateWindow mainWin $ (resizeWindow (fst mwdim) (snd mwdim)) -- >> (moveWindow ((fst msdim)+3) 1)
  updateBorders stdscr y_x_width
  drawTab mainWin $ getCurrentDisplay map1 (0,0) (y_x_width)
  drawClearMsg msgWin "Resized"

useInput game s = drawClearMsg (msgWin game) (show s) -- Any other input

-- Draw a message on the window (clear all before)
drawClearMsg :: Window -> String -> Curses ()
drawClearMsg win str = updateWindow win $ do
  clear
  drawString str

-- Draw borders of a rectangle starting at (pos_x,pos_y) with y rows and x columns on win
makeBorders :: Window -> (Integer,Integer) -> Integer -> Integer -> Curses ()
makeBorders win (pos_y,pos_x) y x = updateWindow win $ do
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
  moveCursor (pos_y) (pos_x+x-1)
  drawGlyph glyphCornerUR
  moveCursor (pos_y+1) (pos_x+x-1)
  drawLineV (Just glyphLineV) $ y-2 -- Vertical Right line
  moveCursor pos_y pos_x -- Reset cursor pos

calculateMainWinSize :: (Integer, Integer) -> (Integer, Integer)
calculateMainWinSize y_x_width = (((fst y_x_width) - msgWin_height-2),((snd y_x_width)-2))

calculateMsgWinSize :: (Integer, Integer) -> (Integer, Integer)
calculateMsgWinSize y_x_width = ((msgWin_height - 2),((snd y_x_width)-2))

updateBorders :: Window -> (Integer,Integer) -> Curses ()
updateBorders stdscr y_x_width = do
  updateWindow stdscr clear
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  makeBorders stdscr (0,0) ((fst msdim) +2) ((snd msdim)+2) -- Make borders of msgWin
  makeBorders stdscr (msgWin_height,0) ((fst mwdim) +2) ((snd mwdim)+2) -- Make borders of mainWin

drawTab :: Window -> [String] -> Curses ()
drawTab win tab = drawClearMsg win $ intercalate "\n" tab
