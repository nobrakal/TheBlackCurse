import UI.NCurses
import System.Exit
import System.Environment
import System.IO.Error
import Data.ConfigFile
import Control.Exception
import Control.Monad

import LevelMap
import Draw
import Keyboard

-- NOTE: Curses is a wrapper for IO

msgWin_height :: Int
msgWin_height = 5

data Game = Game {
  stdscr :: Window,
  mainWin :: Window,
  msgWin :: Window,
  m :: LevelMap,
  keyboard :: Keyboard
}

data State = State {
  game :: Game,
  todo :: Maybe (Curses ())
}

main :: IO ()
main = do
  args <- getArgs -- BUG dessous
  e <- tryJust (guard . isDoesNotExistError) (readFile $ if (1==length args) then (head args) else "../maps/map1.txt")
  let file = either (return ".") id e
  map1' <- loadMap file

  runCurses $ do --Start
    setEcho False -- Disable echo
    setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    y_x_width <- getScreenSize

    updateBorders stdscr y_x_width

    let msdim = calculateMsgWinSize y_x_width
    let mwdim = calculateMainWinSize y_x_width
    msgWin <- newWindow (toInteger $ y msdim) (toInteger $ x msdim) 1 1 -- msg window
    mainWin <- newWindow (toInteger $ y mwdim) (toInteger $ x mwdim) (toInteger $ msgWin_height+1) 1 -- bottom window

    let action = Just $ drawClearMsg msgWin $ either (const "Map not found") (const "Welcome") e

    let map1 = (LevelMap (levelMap (map1')) (Point 0 0) mwdim (maxyx map1')) --Init the map with screen size

    let keyboard = defaultKeyboard

    moveCamera mainWin map1
    render

    mainLoop (State (Game stdscr mainWin msgWin map1 keyboard) action) -- Run mainLoop

mainLoop :: State -> Curses ()
mainLoop (State game (Just todo))= do
  todo
  render
  inp <- getEvent (stdscr game) Nothing
  mainLoop (useInput game inp)

mainLoop (State _ Nothing) = return ()

useInput :: Game -> Maybe Event -> State
useInput game (Just (EventCharacter c)) = useInputKeyboard game (EventCharacter c)
useInput game (Just (EventSpecialKey s)) = useInputKeyboard game (EventSpecialKey s)
useInput game (Just (EventResized)) = State game $ Just $ updateScreenSize game
useInput game (Just (EventUnknown s)) = State game $ Just $ drawClearMsg (msgWin game) $"ERROR WITH EVENT" ++ show s  -- ERROR
useInput game s = State game $ Just $ drawClearMsg (msgWin game) (show s)  -- Any other input

useInputKeyboard :: Game -> Event -> State
useInputKeyboard (Game stdscr mainWin msgWin map1 k) e
  | elem e [cUp k, cDown k, cLeft k, cRight k] = testAndMove (Game stdscr mainWin msgWin map1 k) e
  | e == help k = State (Game stdscr mainWin msgWin map1 k) $ Just $ drawClearMsg msgWin (show k)
  | e == exit k = State (Game stdscr mainWin msgWin map1 k) Nothing
  | otherwise = State (Game stdscr mainWin msgWin map1 k) $ Just $ drawClearMsg msgWin "Command not found"

updateScreenSize :: Game -> Curses ()
updateScreenSize (Game stdscr mainWin msgWin map1 _) =  do
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

-- Test if we can move the camera then does it else say it cannot
testAndMove :: Game -> Event -> State
testAndMove (Game stdscr mainWin msgWin (LevelMap m currul currbr maxyx ) k) s =
  let newul = addPoint (getDir k s) currul
      newbr = addPoint (getDir k s) currbr
  in let isOk = isOnDisplayableMap (LevelMap m currul currbr maxyx) newul
    in let posOkUl = if isOk then newul else currul
           posOkBr = if isOk then newbr else currbr
           action = if isOk
                      then Just $ (moveCamera mainWin (LevelMap m newul currbr maxyx)) >> drawClearMsg msgWin "Camera moved"
                      else Just $ drawClearMsg msgWin "Could not move the camera"
                      in State (Game stdscr mainWin msgWin (LevelMap m posOkUl posOkBr maxyx) k) action

-- Move the camera (do not do any test)
moveCamera :: Window -> LevelMap -> Curses()
moveCamera win (LevelMap map1 p _ _) = getScreenSize >>= \arg -> drawTab win $ getCurrentDisplay map1 p (calculateMainWinSize arg)

getDir :: Keyboard -> Event -> Point
getDir k s
  | s== up k || s == cUp k  = Point (-1) 0
  | s== down k || s == cDown k  = Point 1 0
  | s== left k || s == cLeft k  = Point 0 (-1)
  | s== right k || s == cRight k = Point 0 1
  | otherwise = Point 0 0

getScreenSize :: Curses Point
getScreenSize = screenSize >>= \arg -> (return (Point (fromIntegral (fst arg)) (fromIntegral (snd arg))))
