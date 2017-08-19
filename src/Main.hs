import UI.NCurses
import System.Exit
import System.Environment
import System.IO.Error
import Data.ConfigFile
import Data.Maybe
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

    let map1 = loadMap file (Point 0 0) --Init the map with screen size
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
  y_x_width <- getScreenSize
  mainLoop (useInput game y_x_width inp)

mainLoop (State _ Nothing) = return ()

useInput :: Game -> Point -> Maybe Event -> State
useInput game _ (Just (EventCharacter c)) = useInputKeyboard game (EventCharacter c)
useInput game _ (Just (EventSpecialKey s)) = useInputKeyboard game (EventSpecialKey s)
useInput game y_x_width (Just (EventResized)) = updateScreenSize game y_x_width
useInput game _ (Just (EventUnknown s)) = State game $ Just $ drawClearMsg (msgWin game) $"ERROR WITH EVENT" ++ show s  -- ERROR
useInput game _ s = State game $ Just $ drawClearMsg (msgWin game) (show s)  -- Any other input

useInputKeyboard :: Game -> Event -> State
useInputKeyboard game@(Game _ mainWin msgWin _ k _ rules) e
  | elem e [cUp k, cDown k, cLeft k, cRight k] = testAndMoveC game $ getDir k e
  | elem e [up k, down k, left k, right k] = testAndMoveP game $ getDir k e
  | e == action k = testAndDoSomething game Nothing
  | e == help k = State game $ Just $ drawClearMsg msgWin (show k) --TODO
  | e == exit k = State game Nothing
  | otherwise = State game $ Just $ drawClearMsg msgWin $ "Command not found: " ++ show e

updateScreenSize :: Game -> Point -> State
updateScreenSize game@(Game stdscr mainWin msgWin lm@(LevelMap m currul@(Point yul xul)) a b c) y_x_width@(Point y' x')= State (Game stdscr mainWin msgWin (LevelMap m currul) a b c) $ Just $ do
  updateWindow mainWin clear
  updateWindow msgWin clear
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  updateWindow msgWin $ resizeWindow (toInteger $y msdim) (toInteger $x msdim)
  updateWindow mainWin $ resizeWindow (toInteger $y mwdim) (toInteger $x mwdim)
  updateBorders stdscr y_x_width
  updateCamera mainWin (LevelMap m currul)
  drawClearMsg msgWin "Resized"
  where
    newbr = (Point (yul+y') (xul+x'))

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
testAndMoveC (Game stdscr mainWin msgWin lm@(LevelMap m currul@(Point cy cx)) k player rules) s =
  let newul@(Point ny nx) = addPoint currul $ dirToPoint s
      -- Can do better in this test.......
  in let isOk = isOnDisplayableMap lm newul
    in let posOkUl = if isOk then newul else currul
           action = if isOk
                      then Just $ (updateCamera mainWin (LevelMap m posOkUl)) >> drawClearMsg msgWin "Camera moved"
                      else Just $ drawClearMsg msgWin "Could not move the camera"
                      in State (Game stdscr mainWin msgWin (LevelMap m posOkUl) k player rules) action

-- Test and run the player move
testAndMoveP :: Game -> Direction -> State
testAndMoveP game@(Game stdscr mainWin msgWin lm@(LevelMap map1 po) k p@(Beast pos dir pv) rules) s =
  let newpos = addPoint pos $ dirToPoint s
  in let isOk = (isOnDisplayableMap (LevelMap map1 po) newpos) && canGoTrough lm newpos rules
    in let poskOkPlayer = if isOk then newpos else pos
          -- TODO Test moveCat
           newmap = moveCAtPos (y poskOkPlayer) (x poskOkPlayer) '@' $ (invertAtIndex (y pos) (x pos)  map1)
           in if isOk
                then testAndDoSomething (Game stdscr mainWin msgWin (LevelMap newmap po) k (Beast poskOkPlayer s pv) rules) $ Just $ updateCamera mainWin (LevelMap newmap po) >> drawClearMsg msgWin "Player moved"
                else testAndDoSomething (Game stdscr mainWin msgWin (LevelMap map1 po) k (Beast poskOkPlayer s pv) rules) Nothing

-- Move the camera (do not do any test)
updateCamera :: Window ->  LevelMap -> Curses()
updateCamera win (LevelMap map1 p ) = getScreenSize >>= \x -> drawTab win (calculateMainWinSize x) $ getCurrentDisplay map1 p (calculateMainWinSize x)

-- Test if can do something, and if possible actually do it
testAndDoSomething :: Game -> Maybe (Curses ()) -> State
testAndDoSomething game@(Game stdscr mainWin msgWin lm@(LevelMap map1 po ) k p@(Beast pos dir pv) rules) action
  |canInteractWith lm (addPoint pos $ dirToPoint dir) rules = doSomethingAt game action' $ addPoint pos $ dirToPoint dir
  |otherwise = if isJust action then State game action else State game $ Just $ action' >> (drawClearMsg msgWin "Cannot do anything")
  where
    action' = if isJust action then fromJust action else return ()

doSomethingAt :: Game -> Curses ()-> Point -> State
doSomethingAt game@(Game stdscr mainWin msgWin lm@(LevelMap map1 po) k p@(Beast pos dir pv) rules) action p'
  | poss == 'K' = actDo "The man walked and say nothing"
  | poss == 'm' = actDo "This would burn"
  | poss == '*' = actDo "Trees evrywhere"
  | otherwise = actDo "Would interract"
  where
    poss = head $ getCellAt map1 p'
    willSay' = \x -> willSay rules map1 p' x
    actDo = \x -> State game $ Just $ action >> (drawClearMsg msgWin (willSay' x))
