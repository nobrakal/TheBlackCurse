{-# LANGUAGE TemplateHaskell #-}

import UI.NCurses
import System.Exit
import System.Environment
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.ConfigFile
import Data.Maybe
import Data.List (delete, elemIndex)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Space
import LevelMap
import Draw
import Keyboard
import Beast
import Monsters
import Dialogue
import GameTypes
import Files

-- NOTE: Curses is a wrapper for IO

$(buildKeyboard' "buildKeyboard" getEvents)

main :: IO ()
main = do
  args <- getArgs

  let rulesPath = if 2<=length args then head $ tail args else "./maps/map1.rules"
  let mapPath = if 1<=length args then head args else "./maps/map1.txt"
  let confPath = if length args >= 3 then Just (args !! 2) else Nothing

  fileRules <- loadF rulesPath
  configFile <- maybe (return emptyCP) loadC confPath
  (b,map1) <- loadM mapPath fileRules

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

    render

    mainLoop $ start stdscr mainWin msgWin rulesPath mapPath confPath fileRules configFile (b, map1) -- Run mainLoop

mainLoop :: State -> Curses ()
mainLoop (State _ _ Quit _) = return ()

mainLoop (State com@(Common stdscr mainWin msgWin mapPath rulesPath confPath k) _ Load _) = do
  fileRules <- liftIO $ loadF rulesPath
  (b,map1) <- liftIO $ loadM mapPath fileRules
  configFile <- liftIO $ maybe (return emptyCP) loadC confPath
  mainLoop $ start stdscr mainWin msgWin rulesPath mapPath confPath fileRules configFile (b, map1)

mainLoop (State common' game' status todo')= do
  todo'
  --drawClearMsg (msgWin common') $ show status
  render
  inp <- getEvent (stdscr common') Nothing
  y_x_width <- getScreenSize
  mainLoop $ useInput common' game' status y_x_width inp

-- Use the input
useInput :: Common -> Game -> Status -> Point -> Maybe Event -> State
useInput com game status y_x_width (Just e) = case e of
  (EventCharacter c) -> useInputSwitchStatus com game (EventCharacter c) status $ calculateMainWinSize (rules game) y_x_width
  (EventSpecialKey s) -> useInputSwitchStatus com game (EventSpecialKey s) status $ calculateMainWinSize (rules game) y_x_width
  EventResized -> State com game MainGame $ updateScreenSize com game y_x_width
  (EventUnknown s) -> State com game MainGame $ drawClearMsg (msgWin com) $"ERROR WITH EVENT" ++ show s  -- ERROR
  s@_ -> State com game MainGame $ drawClearMsg (msgWin com) (show s)  -- Any other input
useInput com g status _ s = State com g MainGame $ drawClearMsg (msgWin com) (show s)  -- Any other input

-- Switch between Dialogue and MainGame input usage
useInputSwitchStatus :: Common -> Game -> Event -> Status -> Point -> State
useInputSwitchStatus c g e status p  = case status of
  MainGame -> useInputKeyboardMG c g e p
  InDialogue -> useInputKeyboardD c g e p
  Dead -> useInputKeyboardDead c g e p

useInputKeyboardMG :: Common -> Game -> Event -> Point -> State
useInputKeyboardMG com@(Common _ mainWin msgWin mapPath rulesPath _ k) game e y_x_width
  | e `elem` [cUp k, cDown k, cLeft k, cRight k] = testAndMoveC com game (getDir k e) y_x_width
  | e `elem` [up k, down k, left k, right k] = todoMonsters $ testAndMoveP com game (getDir k e) y_x_width
  | e == action k = todoMonsters $ testAndDoSomething (basestate Action (return ())) y_x_width
  | e == view k = basestate MainGame $ drawClearMsg msgWin $ getStatus (player game)
  | e == load k = basestate Load $ return ()
  | e == help k = basestate InDialogue $ drawClearMsg msgWin (show k) --TODO
  | e == save k = basestate MainGame $ liftIO (writeFile mapPath (toStr $ levelMap $ m game) >> writeFile rulesPath (to_string $ either (const $ rules game) id $ set (rules game) "GAME" "currul" $ show $ currul (m game))) >> drawClearMsg msgWin "Saving..."
  | e == exit k = basestate Quit $ return ()
  | otherwise = basestate MainGame $ drawClearMsg msgWin $ "Command not found: " ++ show e
  where
    basestate = State com game

useInputKeyboardD :: Common -> Game -> Event -> Point -> State
useInputKeyboardD com@(Common _ mainWin msgWin _ _ _ k) game@(Game _ _ _ rules' d@(Dialogue str pos section options lastoption)) e p
  | e == exit k = basestate MainGame $ drawClearMsg msgWin "Exiting the dialogue..."
  | isJust options && e `elem` take (length $ fromJust options) [one k, two k, three k, four k, five k] = runChoiceDialogue com game e p
  | e `elem` [up k, cUp k, down k, cDown k] = State com (game {rules =setOrUnsetLastoption rules' section lastoption, dialogue = d {charpos =  newpos}}) isInDialogue action
  | otherwise = basestate InDialogue $ drawClearMsg msgWin $"Command not found. Please exit the dialogue before (press " ++ show (exit k) ++ ")"
  where
    newpos = pos >>= \y -> updateWindow msgWin $ getWindowSize >>= \x -> return (getNewStartDialogue str y (getDir k e) x)
    msg = calculateMsgWinSize rules' p
    isInDialogue = if isNothing options && length str < x msg * y msg then MainGame else InDialogue
    basestate = State com game
    action = pos >>= \y -> updateWindow msgWin $ getWindowSize >>= \x -> drawClearMsg' x $ showDialogue d y (getDir k e) p

useInputKeyboardDead :: Common -> Game -> Event -> Point -> State
useInputKeyboardDead com@(Common _ mainWin msgWin _ _ _ k) game@(Game _ _ _ rules' d@(Dialogue str pos section options lastoption)) e p
  | e == exit k = basestate Quit $ return ()
  | otherwise = basestate Dead $ drawClearMsg msgWin "You can only quit for now"
  where
    basestate = State com game

runChoiceDialogue :: Common -> Game -> Event -> Point -> State
runChoiceDialogue com@(Common _ mainWin msgWin _ _ _ k) game@(Game _ _ _ rules' d@(Dialogue str pos section options lastoption)) e p
  | e == one k = run 1
  | e == two k = run 2
  | e == three k = run 3
  | e == four k = run 4
  | e == five k = run 5
  where
    run x' =useInputKeyboardD com (game {dialogue = newDialogue rules' (fst $ fromJust options !! (x'-1)) section False,
      rules = setOrUnsetLastoption rules' section lastoption }) (up k) p

updateScreenSize :: Common -> Game -> Point -> Curses ()
updateScreenSize (Common stdscr mainWin msgWin _ _ _ _ ) game y_x_width =  do
  updateWindow mainWin clear
  updateWindow msgWin clear
  let msdim = calculateMsgWinSize (rules game) y_x_width
  let mwdim = calculateMainWinSize (rules game) y_x_width
  updateWindow msgWin $ resizeWindow (toInteger $ y msdim) (toInteger $ x msdim)
  updateWindow mainWin $ resizeWindow (toInteger $ y mwdim) (toInteger $ x mwdim)
  updateBorders (rules game) stdscr y_x_width
  updateCamera mainWin game
  drawClearMsg msgWin "Resized"

updateBorders :: ConfigParser -> Window -> Point -> Curses ()
updateBorders c stdscr y_x_width = do
  updateWindow stdscr clear
  let msdim = calculateMsgWinSize c y_x_width
  let mwdim = calculateMainWinSize c y_x_width
  makeBorders stdscr (Point 0 0) (Point (y msdim +2) (x msdim+2)) -- Make borders of msgWin
  makeBorders stdscr (Point (msgWinHeight c) 0) (Point (y mwdim +2) (x mwdim+2) )-- Make borders of mainWin

-- Test if we can move the camera then does it else say it cannot
testAndMoveC :: Common -> Game -> Direction -> Point -> State
testAndMoveC com game@(Game lm@(LevelMap _ currul) p _ _ _ ) s winsize = State com game {m = lm {currul = if isOk then newul else currul}, player = p {look = s}} MainGame action
  where
    newul@(Point ny nx) = currul + dirToPoint s
    isOk = isOnDisplayableMap lm newul && isOnDisplayableMap lm (newul + winsize + Point (-1) (-1))
    action = if isOk
      then updateCamera (mainWin com) (game {m = lm {currul = newul}}) >> drawClearMsg (msgWin com) "Camera moved"
      else drawClearMsg (msgWin com) "Could not move the camera"

-- Test and run the player move
testAndMoveP :: Common -> Game -> Direction -> Point -> State
testAndMoveP com@(Common stdscr mainWin msgWin _ _ _ k) game@(Game lm@(LevelMap map1 po) b _ rules _ ) s winsize = if isOk
  then testAndDoSomething (basestate $ updateCamera mainWin g>> drawClearMsg msgWin "Player moved") winsize
  else testAndDoSomething (basestate $ drawClearMsg msgWin "Cannot do anything") winsize
  where
    newpos = pos b + dirToPoint s
    isOk = isOnDisplayableMap (LevelMap map1 po) newpos && canGoTrough lm newpos rules
    poskOkPlayer = if isOk then newpos else pos b
    newmap = moveCAtPos (y poskOkPlayer) (x poskOkPlayer) '@' $ removeFirstCharAt (y $ pos b) (x $ pos b)  map1
    g = game { player = b {pos=poskOkPlayer,look=s}, m = lm {levelMap = newmap}}
    basestate = State com g MainGame

-- Test if can do something, and if possible actually do it
testAndDoSomething :: State -> Point -> State
testAndDoSomething (State com game@(Game lm@(LevelMap map1 _ ) p@(Beast pos dir _ _ _ _) _ rules _) status action) p' = case status of
  MainGame -> basestate $ if canInteractWith lm newpos rules "tosay" then action >> drawClearMsg (msgWin com) (willDo' "Would speak with") else action
  Action | canInteractWith lm newpos rules "dialogue" && not (isEnded rules section) -> useInputKeyboardD com (game {dialogue = newDialogue rules "dialogue" section True }) (up $ keyboard com) p'
    | canInteractWith lm newpos rules "hp" -> hitMonster com game newpos
    | otherwise -> basestate $ drawClearMsg (msgWin com) "Cannot do anything"
  where
    newpos = pos + dirToPoint dir
    willDo' =  findWithPrefix rules "tosay" (getCellAt map1 newpos)
    basestate = State com game MainGame
    section = getCellAt map1 newpos

start :: Window -> Window -> Window -> FilePath -> FilePath -> Maybe FilePath -> ConfigParser -> ConfigParser -> (Bool, LevelMap) -> State
start stdscr mainWin msgWin rulesPath mapPath confPath fileRules configFile (b, map1)= State (Common stdscr mainWin msgWin mapPath rulesPath confPath (buildKeyboard $ merge defaultKeyboard configFile)) game MainGame (drawClearMsg msgWin (if b then "Welcome" else "Map not found") >> updateCamera mainWin game)
  where
   charpos = getCharPos (levelMap map1) '@' 0 0
   player = Beast charpos DOWN (either (const 10) id $ get fileRules "PLAYER" "hp") (either (const 2) id $ get fileRules "PLAYER" "dammage") 0 "Player"
   game = Game map1 player (findMonsters map1 fileRules) fileRules (newDialogue fileRules "" "DEFAULT" True)
