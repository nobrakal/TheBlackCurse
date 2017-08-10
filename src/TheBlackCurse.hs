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
  m :: LevelMap
}

data State = State {
  game :: Game,
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

    mainLoop (State (Game stdscr mainWin msgWin map1) $ Just $ drawClearMsg msgWin "Welcome") -- Run mainLoop

mainLoop :: State -> Curses ()
mainLoop (State game (Just todo))= do
  todo
  render
  inp <- getEvent (stdscr game) Nothing
  mainLoop (useInput game inp)

mainLoop (State _ Nothing) = return ()

useInput :: Game -> Maybe Event -> State
useInput game (Just (EventCharacter c))
  | c=='Q' || c=='q' || c=='\ESC' = State game Nothing
useInput (Game stdscr mainWin msgWin (LevelMap map1 (cx, cy))) (Just (EventSpecialKey s)) -- BUG Ajouter tests sur positions
  | (s==KeyUpArrow)= State (Game stdscr mainWin msgWin (LevelMap map1 (cx-1, cy))) (Just (moveCamera mainWin (LevelMap map1 (cx, cy)) (-1,0)))
  | (s==KeyDownArrow)= State (Game stdscr mainWin msgWin (LevelMap map1 (cx+1, cy))) (Just (moveCamera mainWin (LevelMap map1 (cx, cy)) (1,0)))
  | (s==KeyLeftArrow)= State (Game stdscr mainWin msgWin (LevelMap map1 (cx, cy-1))) (Just (moveCamera mainWin (LevelMap map1 (cx, cy)) (0,-1)))
  | (s==KeyRightArrow)= State (Game stdscr mainWin msgWin (LevelMap map1 (cx, cy+1))) (Just (moveCamera mainWin (LevelMap map1 (cx, cy)) (0,1)))
useInput game (Just (EventResized)) = State game $ Just $ updateScreenSize game
useInput game (Just (EventUnknown s)) = State game $ Just $ drawClearMsg (msgWin game) $"ERROR WITH EVENT" ++ show s  -- ERROR
useInput game s = State game $ Just $ drawClearMsg (msgWin game) (show s)  -- Any other input

updateScreenSize :: Game -> Curses ()
updateScreenSize (Game stdscr mainWin msgWin map1) =  do
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
moveCamera :: Window -> LevelMap -> (Integer,Integer) -> Curses()
moveCamera win (LevelMap map1 (cy,cx)) (y,x) = screenSize >>= \arg -> (drawTab win $ getCurrentDisplay map1 ((y+cy),(x+cx)) (calculateMainWinSize arg))
