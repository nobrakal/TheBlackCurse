import UI.NCurses
import System.Exit

-- NOTE: Curses is a wrapper for IO

main :: IO ()
main =
  runCurses $ do --Start
    setEcho False -- Disable echo
    setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    let msgWin_width = 5
    y_x_width <- screenSize

    msgWin <- newWindow (msgWin_width - 2) ((snd y_x_width)-2) 1 1-- msg window
    mainWin <- newWindow ((fst y_x_width) - msgWin_width-2) ((snd y_x_width)-2) (msgWin_width+1) 1 -- bottom window

    makeBorders stdscr (0,0) (msgWin_width) (snd y_x_width) -- Make borders of msgWin
    makeBorders stdscr (msgWin_width,0) ((fst y_x_width) - msgWin_width-1) (snd y_x_width) -- Make borders of mainWin

    render

    mainLoop stdcr mainWin msgWin

mainLoop :: Window -> Window -> Window -> Curses ()
mainLoop stdscr mainWin msgWin = do
  inp <- getEvent stdscr Nothing

  if (inp == (Just (EventCharacter 'q'))) || (inp == (Just (EventCharacter 'Q'))) || (inp == (Just (EventCharacter '\ESC'))) then
   return ()
  else
    useInput msgWin inp >>
    render >>
    mainLoop stdscr mainWin msgWin


useInput :: Window -> Maybe Event -> Curses ()
useInput win (Just (EventSpecialKey s))
  | (s==KeyUpArrow) || (s==KeyDownArrow) || (s==KeyLeftArrow) || (s==KeyRightArrow) = drawMsg win "A direction was pressed" -- moveCharacter s
useInput win (Just (EventUnknown s)) = drawMsg win $ "ERROR WITH EVENT" ++ show s
useInput win s = drawMsg win (show s)

-- Draw a message on the msg window (clear all before)
drawMsg :: Window -> String -> Curses ()
drawMsg msgWin str = updateWindow msgWin $ clear >> (drawString str)

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
