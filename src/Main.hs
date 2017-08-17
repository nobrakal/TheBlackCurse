import UI.NCurses
import System.Exit
import System.Environment
import System.IO.Error
import Data.ConfigFile
import Control.Exception
import Control.Monad

import Space
import LevelMap
import Draw
import Keyboard
import Beast

-- NOTE: Curses is a wrapper for IO

msgWin_height :: Int
msgWin_height = 5

data Game = Game {
  stdscr :: Window,
  mainWin :: Window,
  msgWin :: Window,
  m :: LevelMap,
  keyboard :: Keyboard,
  player :: Beast,
  rules :: ConfigParser
}

data State = State {
  game :: Game,
  todo :: Maybe (Curses ())
}

main :: IO ()
main = do
  args <- getArgs

  -- Load the map file
  e <- tryJust (guard . isDoesNotExistError) (readFile $ if (1<=length args) then (head args) else "./maps/map1.txt")
  let file = either (return ".") id e

  -- Load the rule file
  fr <- readfile emptyCP $ if (2<=length args) then (head $ tail args) else "./maps/map1.rules"
  let fileRules = either (return emptyCP) id fr

  -- Load the config file if provided
  cp <- if (3 == length args) then (readfile emptyCP (args !! 2)) else return (return emptyCP)
  let configFile = either (return emptyCP) id cp

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

    let map1 = loadMap file (Point 0 0) mwdim --Init the map with screen size
    let action = Just $ drawClearMsg msgWin $ either (const "Map not found") (const "Welcome") e
    let keyboard = loadKeyboard $ merge defaultKeyboard configFile

    let player = Beast (getCharPos (levelMap map1) '@' 0 0) DOWN 10

    updateCamera mainWin map1
    render

    mainLoop (State (Game stdscr mainWin msgWin map1 keyboard player fileRules) action) -- Run mainLoop

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
useInputKeyboard game@(Game _ mainWin msgWin _ k _ rules) e
  | elem e [cUp k, cDown k, cLeft k, cRight k] = testAndMoveC game $ getDir k e
  | elem e [up k, down k, left k, right k] = testAndMoveP game $ getDir k e
  | e == action k = doSomething game
  | e == help k = State game $ Just $ drawClearMsg msgWin (show k) --TODO
  | e == exit k = State game Nothing
  | otherwise = State game $ Just $ drawClearMsg msgWin $ "Command not found: " ++ show e

updateScreenSize :: Game -> Curses ()
updateScreenSize (Game stdscr mainWin msgWin map1 _ _ _) =  do
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
testAndMoveC :: Game -> Direction -> State
testAndMoveC (Game stdscr mainWin msgWin lm@(LevelMap m currul@(Point cy cx) currbr@(Point sy sx) maxyx) k player rules) s =
  let newul@(Point ny nx) = addPoint currul $ dirToPoint s
      newbr = addPoint currbr $ dirToPoint s
  in let isOk = isOnDisplayableMap lm (Point (ny-cy+sy) (nx-cx+sx))
    in let posOkUl = if isOk then newul else currul
           posOkBr = if isOk then newbr else currbr
           action = if isOk
                      then Just $ (updateCamera mainWin (LevelMap m newul currbr maxyx)) >> drawClearMsg msgWin "Camera moved"
                      else Just $ drawClearMsg msgWin "Could not move the camera"
                      in State (Game stdscr mainWin msgWin (LevelMap m posOkUl posOkBr maxyx) k player rules) action

-- Test and run the player move
testAndMoveP :: Game -> Direction -> State
testAndMoveP game@(Game stdscr mainWin msgWin lm@(LevelMap map1 po m maxyx) k p@(Beast pos dir pv) rules) s =
  let newpos = addPoint pos $ dirToPoint s
  in let isOk = (isOnDisplayableMap (LevelMap map1 po m (addPoint maxyx (Point (-1) (-1)))) newpos) && canGoTrough lm newpos
    in let poskOkPlayer = if isOk then newpos else pos
          -- TODO Test moveCat
           newmap = moveCAtPos (y poskOkPlayer) (x poskOkPlayer) '@' $ (invertAtIndex (y pos) (x pos)  map1)
           in let action = if isOk
                            then Just $ updateCamera mainWin (LevelMap newmap po m maxyx) >> drawClearMsg msgWin "Player moved"
                            else Just $ drawClearMsg msgWin "Could not move the player"
                            in State (Game stdscr mainWin msgWin (LevelMap newmap po m maxyx) k (Beast poskOkPlayer s pv) rules) action

-- Move the camera (do not do any test)
updateCamera :: Window -> LevelMap -> Curses()
updateCamera win (LevelMap map1 p _ _) = getScreenSize >>= \arg -> drawTab win $ getCurrentDisplay map1 p (calculateMainWinSize arg)

doSomething :: Game -> State
doSomething game@(Game stdscr mainWin msgWin lm@(LevelMap map1 po m maxyx) k p@(Beast pos dir pv) rules) = State game Nothing
