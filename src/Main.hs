import UI.NCurses
import System.Exit
import System.Environment
import System.IO.Error
import Data.ConfigFile
import Data.Maybe
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Space
import LevelMap
import Draw
import Keyboard
import Beast
import Dialogue

-- NOTE: Curses is a wrapper for IO

data Common = Common {
  stdscr :: Window,
  mainWin :: Window,
  msgWin :: Window,
  mapPath :: FilePath,
  rulesPath :: FilePath,
  keyboard :: Keyboard
}

data Game = Game {
  m :: LevelMap,
  player :: Beast,
  rules :: ConfigParser,
  dialogue :: Dialogue
}

data State = State {
  common :: Common,
  game :: Game,
  status :: Status,
  todo :: Maybe (Curses ())
}

data Status = MainGame | InDialogue

main :: IO ()
main = do
  args <- getArgs

  -- Load the map file
  let mapPath = if 1<=length args then head args else "./maps/map1.txt"
  e <- tryJust (guard . isDoesNotExistError) (readFile mapPath)
  let file = either (return ".") id e

  -- Load the rule file
  let rulesPath = if 2<=length args then head $ tail args else "./maps/map1.rules"
  fr <- readfile emptyCP rulesPath
  let fileRules = either (return emptyCP) id fr

  -- Load the config file if provided
  cp <- if 3 == length args then readfile emptyCP (args !! 2) else return (return emptyCP)
  let configFile = either (return emptyCP) id cp

  runCurses $ do --Start
    setEcho False -- Disable echo
    setCursorMode CursorInvisible -- No more cursor
    stdscr <- defaultWindow

    y_x_width <- getScreenSize

    updateBorders fileRules stdscr y_x_width

    let msdim = calculateMsgWinSize fileRules y_x_width
    let mwdim = calculateMainWinSize fileRules y_x_width
    msgWin <- newWindow (toInteger $ y msdim) (toInteger $ x msdim) 1 1 -- msg window
    mainWin <- newWindow (toInteger $ y mwdim) (toInteger $ x mwdim) (toInteger $ msgWinHeight fileRules +1) 1 -- bottom window

    let map1 = loadMap file (Point 0 0) --Init the map with screen size
    let action = Just $ drawClearMsg msgWin $ either (const "Map not found") (const "Welcome") e
    let keyboard = loadKeyboard $ merge defaultKeyboard configFile

    let player = Beast (getCharPos (levelMap map1) '@' 0 0) DOWN 10

    let common = Common stdscr mainWin msgWin mapPath rulesPath keyboard
    let game = Game map1 player fileRules (newDialogue "" "DEFAULT")

    updateCamera mainWin game
    render

    mainLoop (State common game MainGame action) -- Run mainLoop


msgWinHeight :: ConfigParser -> Int
msgWinHeight x = either (const 5) read $ get x "GAME" "msgwinheight"

mainLoop :: State -> Curses ()
mainLoop (State common game status (Just todo) )= do
  todo
  render
  inp <- getEvent (stdscr common) Nothing
  y_x_width <- getScreenSize
  mainLoop (useInput common game status y_x_width inp)

mainLoop (State _ _ _ Nothing) = return ()

useInput :: Common -> Game -> Status -> Point -> Maybe Event -> State
useInput com game status y_x_width (Just e) = case e of
  (EventCharacter c) -> useInputSwitchStatus com game (EventCharacter c) status $ calculateMainWinSize (rules game) y_x_width
  (EventSpecialKey s) -> useInputSwitchStatus com game (EventSpecialKey s) status $ calculateMainWinSize (rules game) y_x_width
  EventResized -> State com game MainGame $ Just $ updateScreenSize com game y_x_width
  (EventUnknown s) -> State com game MainGame $ Just $ drawClearMsg (msgWin com) $"ERROR WITH EVENT" ++ show s  -- ERROR
useInput com g status _ s = State com g MainGame $ Just $ drawClearMsg (msgWin com) (show s)  -- Any other input

useInputSwitchStatus :: Common -> Game -> Event -> Status -> Point -> State
useInputSwitchStatus c g e status p  = case status of
  MainGame -> useInputKeyboardMG c g e p
  InDialogue -> useInputKeyboardD c g e p

useInputKeyboardMG :: Common -> Game -> Event -> Point -> State
useInputKeyboardMG com@(Common _ mainWin msgWin mapPath rulesPath k) game e y_x_width
  | e `elem` [cUp k, cDown k, cLeft k, cRight k] = testAndMoveC com game (getDir k e) y_x_width
  | e `elem` [up k, down k, left k, right k] = testAndMoveP com game (getDir k e) y_x_width
  | e == action k = testAndSayTosay (basestate InDialogue Nothing) y_x_width
  | e == help k = basestate InDialogue $ Just $ drawClearMsg msgWin (show k) --TODO
  | e == saveM k = basestate MainGame $ Just $ liftIO (writeFile mapPath (toStr $ levelMap $ m game) >> writeFile rulesPath (to_string $ rules game)) >>  drawClearMsg msgWin "Saving..."
  | e == exit k = basestate MainGame Nothing
  | otherwise = basestate MainGame $ Just $ drawClearMsg msgWin $ "Command not found: " ++ show e
  where basestate = State com game

useInputKeyboardD :: Common ->  Game -> Event -> Point -> State
useInputKeyboardD  com@(Common _ mainWin msgWin _ _ k) game@(Game _  _ rules d@(Dialogue str pos section options)) e p
  | e == exit k = basestate MainGame $ Just $ drawClearMsg msgWin "Exiting the dialogue..."
  | e `elem` take (length options)[one k, two k, three k, four k, five k] = runChoiceDialogue com game e p
  | e `elem` [up k, cUp k, down k, cDown k] = State com game {dialogue = d {charpos =  newpos}} isInDialogue $ Just $ pos >>= \y -> updateWindow msgWin $ getWindowSize >>= \x -> drawClearMsg' x $ drop (getNewStartDialogue str' y (getDir k e) x) str' ++ "\n" ++ showOptions options
  | otherwise = basestate InDialogue $ Just $ drawClearMsg msgWin $"Please exit the dialogue before (press " ++ show (exit k) ++ ")"
  where
    newpos = pos >>= \y -> updateWindow msgWin $ getWindowSize >>= \x -> return (getNewStartDialogue str y (getDir k e) x)
    str' = getStrPart str
    msg = calculateMsgWinSize rules p
    isInDialogue = if null options && length str < x msg * y msg then MainGame else InDialogue
    basestate = State com game

runChoiceDialogue :: Common -> Game -> Event -> Point -> State
runChoiceDialogue com@(Common _ mainWin msgWin _ _ k) game@(Game _ _ rules d@(Dialogue str pos section options)) e p
  | e == one k = run 1
  | e == two k = run 2
  | e == three k = run 3
  | e == four k = run 4
  | e == five k = run 5
  where
    run x' =useInputKeyboardD com (game {dialogue = newDialogue (either (const "ERROR IN DIALOGUE") id $ get rules section (fst $ options !! (x'-1)) ) section}) (up k) p

updateScreenSize :: Common -> Game -> Point -> Curses ()
updateScreenSize (Common stdscr mainWin msgWin _ _ _ ) game@(Game lm _ rules _) y_x_width =  do
  updateWindow mainWin clear
  updateWindow msgWin clear
  let msdim = calculateMsgWinSize rules y_x_width
  let mwdim = calculateMainWinSize rules y_x_width
  updateWindow msgWin $ resizeWindow (toInteger $y msdim) (toInteger $x msdim)
  updateWindow mainWin $ resizeWindow (toInteger $y mwdim) (toInteger $x mwdim)
  updateBorders rules stdscr y_x_width
  updateCamera mainWin game
  drawClearMsg msgWin "Resized"

calculateMainWinSize :: ConfigParser -> Point -> Point
calculateMainWinSize c (Point y x ) = Point (y - msgWinHeight c -2) (x-2)

calculateMsgWinSize :: ConfigParser -> Point -> Point
calculateMsgWinSize c (Point _ x ) = Point (msgWinHeight c - 2) (x-2)

updateBorders :: ConfigParser -> Window -> Point -> Curses ()
updateBorders c stdscr y_x_width = do
  updateWindow stdscr clear
  let msdim = calculateMsgWinSize c y_x_width
  let mwdim = calculateMainWinSize c y_x_width
  makeBorders stdscr (Point 0 0) (Point (y msdim +2) (x msdim+2)) -- Make borders of msgWin
  makeBorders stdscr (Point (msgWinHeight c) 0) (Point (y mwdim +2) (x mwdim+2) )-- Make borders of mainWin

-- Test if we can move the camera then does it else say it cannot
testAndMoveC :: Common -> Game -> Direction -> Point -> State
testAndMoveC com game@(Game lm@(LevelMap _ currul) _ _ _ ) s winsize =
  let newul@(Point ny nx) = currul + dirToPoint s
  in let isOk = isOnDisplayableMap lm newul && isOnDisplayableMap lm (newul + winsize + Point (-1) (-1))
    in let posOkUl = if isOk then newul else currul
           action = if isOk
                      then Just $ updateCamera (mainWin com) (game {m = lm {currul = newul}}) >> drawClearMsg (msgWin com) "Camera moved"
                      else Just $ drawClearMsg (msgWin com) "Could not move the camera"
                      in State com game {m = lm {currul = posOkUl}} MainGame action

-- Test and run the player move
testAndMoveP :: Common -> Game -> Direction -> Point -> State
testAndMoveP com@(Common stdscr mainWin msgWin _ _ k) game@(Game lm@(LevelMap map1 po) p@(Beast pos dir pv) rules _ ) s winsize =
  let newpos = pos + dirToPoint s
  in let isOk = isOnDisplayableMap (LevelMap map1 po) newpos && canGoTrough lm newpos rules
    in let poskOkPlayer = if isOk then newpos else pos
           newmap = moveCAtPos (y poskOkPlayer) (x poskOkPlayer) '@' $ removeFirstCharAt (y pos) (x pos)  map1
           g = game { player = p {pos=poskOkPlayer,look=s}, m = lm {levelMap = newmap}}
           basestate = State com g MainGame
           in if isOk
                then testAndSayTosay (basestate $ Just $ updateCamera mainWin g>> drawClearMsg msgWin "Player moved") winsize
                else testAndSayTosay (basestate Nothing) winsize

-- Move the camera (do not do any test)
updateCamera :: Window -> Game -> Curses()
updateCamera win (Game (LevelMap map1 p ) (Beast pos _ _) rules _) = getScreenSize >>= \x -> drawTab win (calculateMainWinSize rules x) (getCurrentDisplay actualmap p (calculateMainWinSize rules x))
  where
    radius = either (const 0) read $ get rules "GAME" "radius"
    actualmap = if 0 < radius
      then getRadius map1 rules pos radius
      else map1

-- Test if can do something, and if possible actually do it
testAndSayTosay :: State -> Point -> State
testAndSayTosay (State com@(Common _ _ msgWin _ _ k) game@(Game  lm@(LevelMap map1 _ ) p@(Beast pos dir _) rules _) status action) p' = case status of
  MainGame -> State com game status $ if canInteractWith lm newpos rules "tosay" then Just $ action' >> drawClearMsg msgWin (willDo' "tosay" "Would speak with") else cannot
  InDialogue -> if canInteractWith lm newpos rules "dialogue" then useInputKeyboardD com (game {dialogue = newDialogue (willDo' "dialogue" "Would speak with") (getCellAt map1 newpos) }) (up k) p' else State com game MainGame cannot
  where
    action' = fromMaybe (return ()) action
    newpos = pos + dirToPoint dir
    cannot = if isJust action then action else Just $ action' >> drawClearMsg msgWin "Cannot do anything"
    willDo' =  willDo rules map1 newpos
