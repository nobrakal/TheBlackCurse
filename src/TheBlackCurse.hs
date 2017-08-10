import UI.NCurses
import System.Exit

import LevelMap
import Draw

-- NOTE: Curses is a wrapper for IO

msgWin_height :: Int
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
  map1' <- loadMap "./maps/map1.txt"

  runCurses $ do --Start
    setEcho False -- Disable echo
    setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    y_x_width <- getScreenSize

    updateBorders stdscr y_x_width
    let map1 = (LevelMap (levelMap (map1')) (Point 0 0) y_x_width (maxyx map1')) --Init the map with screen size

    let msdim = calculateMsgWinSize y_x_width
    let mwdim = calculateMainWinSize y_x_width
    msgWin <- newWindow (toInteger $ y msdim) (toInteger $ x msdim) 1 1 -- msg window
    mainWin <- newWindow (toInteger $ y mwdim) (toInteger $ x mwdim) (toInteger $ msgWin_height+1) 1 -- bottom window

    drawTab mainWin $ getCurrentDisplay (levelMap map1) (Point 0 0) mwdim
    render

    mainLoop (State (Game stdscr mainWin msgWin map1) $ Just $ drawClearMsg msgWin (show (maxyx map1))) -- Run mainLoop

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
useInput state (Just (EventSpecialKey s)) = testAndMove state s
useInput game (Just (EventResized)) = State game $ Just $ updateScreenSize game
useInput game (Just (EventUnknown s)) = State game $ Just $ drawClearMsg (msgWin game) $"ERROR WITH EVENT" ++ show s  -- ERROR
useInput game s = State game $ Just $ drawClearMsg (msgWin game) (show s)  -- Any other input

updateScreenSize :: Game -> Curses ()
updateScreenSize (Game stdscr mainWin msgWin map1) =  do
  y_x_width <- getScreenSize
  updateWindow mainWin clear
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  updateWindow msgWin $ resizeWindow (toInteger $y msdim) (toInteger $x msdim)
  updateWindow mainWin $ resizeWindow (toInteger $y mwdim) (toInteger $x mwdim)
  updateBorders stdscr y_x_width
  drawTab mainWin $ getCurrentDisplay (levelMap map1) (Point 0 0) (mwdim)
  drawClearMsg msgWin "Resized"

calculateMainWinSize :: Point -> Point
calculateMainWinSize (Point y x ) = Point (y - msgWin_height-2) (x-2)

calculateMsgWinSize :: Point -> Point
calculateMsgWinSize (Point _ x ) = Point (msgWin_height - 2) (x-2)

updateBorders :: Window -> Point -> Curses ()
updateBorders stdscr y_x_width = do
  updateWindow stdscr clear
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  makeBorders stdscr (Point 0 0) (Point ((y msdim) +2) ((x msdim)+2)) -- Make borders of msgWin
  makeBorders stdscr (Point msgWin_height 0) (Point ((y mwdim) +2) ((x mwdim)+2) )-- Make borders of mainWin

  -- Move the camera if possible
moveCamera :: Window -> LevelMap -> Point -> Curses()
moveCamera win (LevelMap map1 (Point cy cx) _ _) (Point y x) = getScreenSize >>= \arg -> drawTab win $ getCurrentDisplay map1 (Point (y+cy) (x+cx)) (calculateMainWinSize arg)

getDir :: Key -> Point
getDir s
  | s==KeyUpArrow = Point (-1) 0
  | s==KeyDownArrow = Point 1 0
  | s==KeyLeftArrow = Point 0 (-1)
  | s==KeyRightArrow = Point 0 1
  | otherwise = Point 0 0

testAndMove :: Game -> Key -> State
testAndMove (Game stdscr mainWin msgWin (LevelMap m currul currbr maxyx )) s =
  let newul = addPoint (getDir s) currul
      newbr = addPoint (getDir s) currbr
  in let isOk = isOnScreen (LevelMap m currul currbr maxyx) newul
    in let posOkUl = if isOk then newul else currul
           posOkBr = if isOk then newbr else currbr
           action = if isOk
                      then Just $ (moveCamera mainWin (LevelMap m currul currbr maxyx) newul) >> drawClearMsg msgWin "Camera moved"
                      else Just $ drawClearMsg msgWin "Could not move the camera"
                      in State (Game stdscr mainWin msgWin (LevelMap m posOkUl posOkBr maxyx)) action

getScreenSize :: Curses Point
getScreenSize = screenSize >>= \arg -> (return (Point (fromIntegral (fst arg)) (fromIntegral (snd arg))))
