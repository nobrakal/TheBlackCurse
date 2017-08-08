import UI.NCurses
import System.Exit
import Data.Ratio

-- NOTE: Curses is a wrapper for IO

msgWin_height :: Integer
msgWin_height = 5

main :: IO ()
main =
  runCurses $ do --Start
    setEcho False -- Disable echo
    setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    y_x_width <- screenSize

    updateBorders stdscr y_x_width

    let msdim = calculateMsgWinSize y_x_width
    msgWin <- newWindow (fst msdim) (snd msdim) 1 1 -- msg window
    let mwdim = calculateMainWinSize y_x_width
    mainWin <- newWindow (fst mwdim) (snd mwdim) (msgWin_height+1) 1 -- bottom window

    render

    mainLoop stdscr mainWin msgWin

mainLoop :: Window -> Window -> Window -> Curses ()
mainLoop stdscr mainWin msgWin = do
  inp <- getEvent stdscr Nothing

  if (inp == (Just (EventCharacter 'q'))) || (inp == (Just (EventCharacter 'Q'))) || (inp == (Just (EventCharacter '\ESC'))) then
   return ()
  else do
    useInput mainWin msgWin inp
    render
    mainLoop stdscr mainWin msgWin


useInput :: Window -> Window -> Maybe Event -> Curses ()
useInput mainWin msgWin (Just (EventSpecialKey s))
  | (s==KeyUpArrow) || (s==KeyDownArrow) || (s==KeyLeftArrow) || (s==KeyRightArrow) = drawClearMsg msgWin "A direction was pressed" -- moveCharacter s
useInput mainWin msgWin (Just (EventUnknown s)) = drawClearMsg msgWin $ "ERROR WITH EVENT" ++ show s
useInput mainWin msgWin (Just (EventResized)) = do
  y_x_width <- screenSize
  stdscr <- defaultWindow
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  updateWindow msgWin $ resizeWindow (fst msdim) (snd msdim)
  updateWindow mainWin $ moveWindow (fst msdim) 1
  updateWindow mainWin $ resizeWindow (fst mwdim) (snd mwdim)
  updateBorders stdscr y_x_width

useInput mainWin msgWin s = drawClearMsg msgWin (show s)

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
  drawGlyph glyphCornerLR
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
  makeBorders stdscr (msgWin_height,0) ((fst mwdim) +1) ((snd mwdim)+2) -- Make borders of mainWin
