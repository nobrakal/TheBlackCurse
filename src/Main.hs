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
import Dialogue

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
  rules :: ConfigParser,
  dialogue :: Dialogue
}

data State = State {
  game :: Game,
  status :: Status,
  todo :: Maybe (Curses ())
}

data Status = MainGame | InDialogue

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

    let game = (Game stdscr mainWin msgWin map1 keyboard player fileRules (Dialogue "" 0))

    updateCamera game
    render

    mainLoop (State game MainGame action) -- Run mainLoop

mainLoop :: State -> Curses ()
mainLoop (State game status (Just todo) )= do
  todo
  render
  inp <- getEvent (stdscr game) Nothing
  y_x_width <- getScreenSize
  mainLoop (useInput game status y_x_width inp)

mainLoop (State _ _ Nothing) = return ()

useInput :: Game -> Status -> Point -> Maybe Event -> State
useInput game status y_x_width (Just e) = case e of
  (EventCharacter c) -> useInputSwitchStatus game (EventCharacter c) status $ calculateMainWinSize y_x_width
  (EventSpecialKey s) -> useInputSwitchStatus game (EventSpecialKey s) status $ calculateMainWinSize y_x_width
  EventResized -> State game MainGame $ Just $ updateScreenSize game y_x_width
  (EventUnknown s) -> State game MainGame $ Just $ drawClearMsg (msgWin game) $"ERROR WITH EVENT" ++ show s  -- ERROR
useInput game status _ s = State game MainGame $ Just $ drawClearMsg (msgWin game) (show s)  -- Any other input

useInputSwitchStatus :: Game -> Event -> Status -> Point -> State
useInputSwitchStatus g e status p  = case status of
  MainGame -> useInputKeyboardMG g e p
  InDialogue -> useInputKeyboardD g e p

useInputKeyboardMG :: Game -> Event -> Point -> State
useInputKeyboardMG game@(Game _ mainWin msgWin _ k _ _ _) e y_x_width
  | elem e [cUp k, cDown k, cLeft k, cRight k] = testAndMoveC game (getDir k e) (y_x_width)
  | elem e [up k, down k, left k, right k] = testAndMoveP game $ getDir k e
  | e == action k = testAndSayTosay (State game InDialogue Nothing)
  | e == help k = State game InDialogue $ Just $ drawClearMsg msgWin (show k) --TODO
  | e == exit k = State game MainGame Nothing
  | otherwise = State game MainGame $ Just $ drawClearMsg msgWin $ "Command not found: " ++ show e

useInputKeyboardD :: Game -> Event -> Point -> State
useInputKeyboardD game@(Game _ mainWin msgWin _ k _ _ dialogue) e y_x_width
  | e == exit k = State game MainGame $ Just $ drawClearMsg msgWin $ "Exiting the dialogue"
  | otherwise = State game InDialogue $ Just $ drawClearMsg msgWin $ "Command not found: " ++ show e

updateScreenSize :: Game -> Point -> Curses ()
updateScreenSize game@(Game stdscr mainWin msgWin lm _ _ _ _) y_x_width =  do
  updateWindow mainWin clear
  updateWindow msgWin clear
  let msdim = calculateMsgWinSize y_x_width
  let mwdim = calculateMainWinSize y_x_width
  updateWindow msgWin $ resizeWindow (toInteger $y msdim) (toInteger $x msdim)
  updateWindow mainWin $ resizeWindow (toInteger $y mwdim) (toInteger $x mwdim)
  updateBorders stdscr y_x_width
  updateCamera game
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
testAndMoveC :: Game -> Direction -> Point -> State
testAndMoveC game@(Game _ _ msgWin lm@(LevelMap _ currul) _ _ _ _ ) s winsize =
  let newul@(Point ny nx) = currul + (dirToPoint s)
  in let isOk = isOnDisplayableMap lm newul && isOnDisplayableMap lm (newul + winsize + (Point (-1) (-1)))
    in let posOkUl = if isOk then newul else currul
           action = if isOk
                      then Just $ (updateCamera $ game {m = lm {currul = newul}}) >> drawClearMsg msgWin "Camera moved"
                      else Just $ drawClearMsg msgWin "Could not move the camera"
                      in State game {m = lm {currul = posOkUl}} MainGame action

-- Test and run the player move
testAndMoveP :: Game -> Direction -> State
testAndMoveP game@(Game stdscr mainWin msgWin lm@(LevelMap map1 po) k p@(Beast pos dir pv) rules _ ) s =
  let newpos = pos + (dirToPoint s)
  in let isOk = (isOnDisplayableMap (LevelMap map1 po) newpos) && canGoTrough lm newpos rules
    in let poskOkPlayer = if isOk then newpos else pos
           newmap = moveCAtPos (y poskOkPlayer) (x poskOkPlayer) '@' $ (invertAtIndex (y pos) (x pos)  map1)
           g = game { player = p {pos=poskOkPlayer}, m = lm {levelMap = newmap}}
           basestate = State g MainGame
           in if isOk
                then testAndSayTosay (basestate $ Just $ updateCamera g>> drawClearMsg msgWin "Player moved")
                else testAndSayTosay (basestate Nothing)

-- Move the camera (do not do any test)
updateCamera :: Game -> Curses()
updateCamera (Game _ win _  (LevelMap map1 p ) _ (Beast pos _ _) rules _) = getScreenSize >>= \x -> drawTab win (calculateMainWinSize x) (getCurrentDisplay actualmap p (calculateMainWinSize x))
  where
    radius = (either (const 0) read $ get rules "GAME" "radius")
    actualmap = if 0 < radius
      then getRadius map1 rules pos radius
      else map1

-- Test if can do something, and if possible actually do it
testAndSayTosay :: State -> State
testAndSayTosay (State game@(Game _ _ msgWin lm@(LevelMap map1 _ ) _ p@(Beast pos dir _) rules _) status action) = case status of
  MainGame -> State game status $ testAndSayTosay' "tosay" "Would speak with"
  InDialogue -> State (game {dialogue = Dialogue (willDo' "dialogue" "Would speak with") (10) }) status $testAndSayTosay' "dialogue" "Would speak with"
  where
    action' = if isJust action then fromJust action else return ()
    newpos = pos + (dirToPoint dir)
    cannot = if isJust action then action else Just $ action' >> (drawClearMsg msgWin "Cannot do anything")
    testAndSayTosay' = \x y-> if canInteractWith lm newpos rules x then actDo x y else cannot
    willDo' = \x y -> willDo rules map1 newpos x y
    actDo = \x y -> Just $ action' >> (drawClearMsg msgWin (willDo' x y))
