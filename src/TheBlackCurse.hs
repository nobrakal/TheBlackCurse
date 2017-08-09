import UI.NCurses
import System.Exit

import LevelMap
import Draw

-- NOTE: Curses is a wrapper for IO

msgWin_height :: Integer
msgWin_height = 5

data Game = Game {
  stdscr :: Window,
  mainWin :: Window,
  msgWin :: Window,
  m :: LevelMap,
  todo :: Maybe (Curses ())
}

main :: IO ()
main = do
  map1 <- loadMap "./maps/map1.txt"

  runCurses $ do --Start
    setEcho False -- Disable echo
    setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    y_x_width <- screenSize

    updateBorders stdscr y_x_width

    let msdim = calculateMsgWinSize y_x_width
    let mwdim = calculateMainWinSize y_x_width
    msgWin <- newWindow (fst msdim) (snd msdim) 1 1 -- msg window
    mainWin <- newWindow (fst mwdim) (snd mwdim) (msgWin_height+1) 1 -- bottom window

    drawTab mainWin $ getCurrentDisplay (levelMap map1) (0,0) (mwdim)
    render

    mainLoop (Game stdscr mainWin msgWin map1 $ Just $ drawClearMsg msgWin "Welcome") -- Run mainLoop

mainLoop :: Game -> Curses ()
mainLoop (Game stdscr mainWin msgWin map1 (Just todo)) = do
  todo
  render
  inp <- getEvent stdscr Nothing
  mainLoop (useInput (Game stdscr mainWin msgWin map1 (Just todo)) inp)

mainLoop (Game stdscr mainWin msgWin map1 Nothing) = return ()

useInput :: Game -> Maybe Event -> Game
useInput (Game stdscr mainWin msgWin map1 todo) (Just (EventCharacter c))
  | c=='Q' || c=='q' || c=='\ESC' = (Game stdscr mainWin msgWin map1 Nothing)
useInput (Game stdscr mainWin msgWin (LevelMap map1 (cx, cy)) todo) (Just (EventSpecialKey s)) -- BUG Ajouter tests sur positions
  | (s==KeyUpArrow)= (Game stdscr mainWin msgWin (LevelMap map1 (cx-1, cy)) (Just (screenSize >>= moveCamera mainWin (LevelMap map1 (cx, cy)) (-1,0))))
  | (s==KeyDownArrow)= (Game stdscr mainWin msgWin (LevelMap map1 (cx+1, cy)) (Just (screenSize >>= moveCamera mainWin (LevelMap map1 (cx, cy)) (1,0))))
  | (s==KeyLeftArrow)= (Game stdscr mainWin msgWin (LevelMap map1 (cx, cy-1)) (Just (screenSize >>= moveCamera mainWin (LevelMap map1 (cx, cy)) (0,-1))))
  | (s==KeyRightArrow)= (Game stdscr mainWin msgWin (LevelMap map1 (cx, cy+1)) (Just (screenSize >>= moveCamera mainWin (LevelMap map1 (cx, cy)) (0,1))))
useInput (Game stdscr mainWin msgWin map1 todo) (Just (EventResized)) = (Game stdscr mainWin msgWin map1 $ Just $ updateScreenSize (Game stdscr mainWin msgWin map1 todo))
useInput (Game stdscr mainWin msgWin map1 todo) (Just (EventUnknown s)) = (Game stdscr mainWin msgWin map1 $ Just $ drawClearMsg msgWin $"ERROR WITH EVENT" ++ show s )  -- ERROR
useInput (Game stdscr mainWin msgWin map1 todo) s = (Game stdscr mainWin msgWin map1 $ Just $ drawClearMsg msgWin (show s))  -- Any other input

updateScreenSize :: Game -> Curses ()
updateScreenSize (Game stdscr mainWin msgWin map1 todo) =  do
  y_x_width <- screenSize
  updateWindow mainWin clear
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  updateWindow msgWin $ resizeWindow (fst msdim) (snd msdim)
  updateWindow mainWin $ (resizeWindow (fst mwdim) (snd mwdim))
  updateBorders stdscr y_x_width
  drawTab mainWin $ getCurrentDisplay (levelMap map1) (0,0) (mwdim)
  drawClearMsg msgWin "Resized"

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

  -- Move the camera if possible
moveCamera :: Window -> LevelMap -> (Integer,Integer) -> (Integer,Integer) -> Curses()
moveCamera win (LevelMap map1 (cy,cx)) (y,x) y_x_stdscr = drawTab win $ getCurrentDisplay map1 ((y+cy),(x+cx)) (calculateMainWinSize y_x_stdscr)
