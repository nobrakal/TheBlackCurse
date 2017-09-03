{-# LANGUAGE TemplateHaskell #-}

import UI.NCurses
import System.Exit
import System.Environment
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.ConfigFile
import Data.Maybe
import Data.List
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

  fileRules <- loadF mapPath
  configFile <- if length args >= 3 then loadC (args !! 2) else return emptyCP
  (b, map1) <- loadM mapPath fileRules

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

    let player = Beast (getCharPos (levelMap map1) '@' 0 0) DOWN (either (const 10) id $ get fileRules "PLAYER" "hp") (either (const 2) id $ get fileRules "PLAYER" "dammage") True "Player"
    let game = Game map1 player (findActivated map1 fileRules) fileRules (newDialogue fileRules "" "DEFAULT" True)

    updateCamera mainWin game
    render

    mainLoop (State (Common stdscr mainWin msgWin mapPath rulesPath (buildKeyboard $ merge defaultKeyboard configFile)) game MainGame (Just $ drawClearMsg msgWin $ if b then "Welcome" else "Map not found")) -- Run mainLoop

msgWinHeight :: ConfigParser -> Int
msgWinHeight x = either (const 5) read $ get x "GAME" "msgwinheight"

mainLoop :: State -> Curses ()
mainLoop (State common' game' status (Just todo'))= do
  let (newplayer, bobo) = todoMonsters common' game' (monsters game') 0
  todo' >> when (status == MainGame && bobo > 0) (drawClearMsg (msgWin common') $ "You were hit, you have now " ++ show (hp (player game')) ++ " hp" )
  when (isDead && status /= Dead) (drawClearMsg (msgWin common') "You are dead")
  render
  inp <- getEvent (stdscr common') Nothing
  y_x_width <- getScreenSize
  mainLoop (useInput common' (game' {player= if status == MainGame then newplayer else player game'}) (if isDead then Dead else status) y_x_width inp)
  where
    isDead = hp (player game') <= 0

mainLoop (State _ _ _ Nothing) = return ()

-- Use the input
useInput :: Common -> Game -> Status -> Point -> Maybe Event -> State
useInput com game status y_x_width (Just e) = case e of
  (EventCharacter c) -> useInputSwitchStatus com game (EventCharacter c) status $ calculateMainWinSize (rules game) y_x_width
  (EventSpecialKey s) -> useInputSwitchStatus com game (EventSpecialKey s) status $ calculateMainWinSize (rules game) y_x_width
  EventResized -> State com game MainGame $ Just $ updateScreenSize com game y_x_width
  (EventUnknown s) -> State com game MainGame $ Just $ drawClearMsg (msgWin com) $"ERROR WITH EVENT" ++ show s  -- ERROR
  s@_ -> State com game MainGame $ Just $ drawClearMsg (msgWin com) (show s)  -- Any other input
useInput com g status _ s = State com g MainGame $ Just $ drawClearMsg (msgWin com) (show s)  -- Any other input

-- Switch between Dialogue and MainGame input usage
useInputSwitchStatus :: Common -> Game -> Event -> Status -> Point -> State
useInputSwitchStatus c g e status p  = case status of
  MainGame -> useInputKeyboardMG c g e p
  InDialogue -> useInputKeyboardD c g e p
  Dead -> useInputKeyboardDead c g e p

useInputKeyboardMG :: Common -> Game -> Event -> Point -> State
useInputKeyboardMG com@(Common _ mainWin msgWin mapPath rulesPath k) game e y_x_width
  | e `elem` [cUp k, cDown k, cLeft k, cRight k] = testAndMoveC com game (getDir k e) y_x_width
  | e `elem` [up k, down k, left k, right k] = testAndMoveP com game (getDir k e) y_x_width
  | e == action k = testAndDoSomething (basestate Action Nothing) y_x_width
  | e == help k = basestate InDialogue $ Just $ drawClearMsg msgWin (show k) --TODO
  | e == save k = basestate MainGame $ Just $ liftIO (writeFile mapPath (toStr $ levelMap $ m game) >> writeFile rulesPath (to_string $ either (const $ rules game) id $ set (rules game) "GAME" "currul" $ show $ currul (m game))) >> drawClearMsg msgWin "Saving..."
  | e == exit k = basestate MainGame Nothing
  | otherwise = basestate MainGame $ Just $ drawClearMsg msgWin $ "Command not found: " ++ show e
  where basestate = State com game

useInputKeyboardD :: Common -> Game -> Event -> Point -> State
useInputKeyboardD com@(Common _ mainWin msgWin _ _ k) game@(Game _ _ _ rules' d@(Dialogue str pos section options lastoption)) e p
  | e == exit k = basestate MainGame $ Just $ drawClearMsg msgWin "Exiting the dialogue..."
  | isJust options && e `elem` take (length $ fromJust options) [one k, two k, three k, four k, five k] = runChoiceDialogue com game e p
  | e `elem` [up k, cUp k, down k, cDown k] = State com (game {rules =setOrUnsetLastoption rules' section lastoption, dialogue = d {charpos =  newpos}}) isInDialogue $ Just action
  | otherwise = basestate InDialogue $ Just $ drawClearMsg msgWin $"Command not found. Please exit the dialogue before (press " ++ show (exit k) ++ ")"
  where
    newpos = pos >>= \y -> updateWindow msgWin $ getWindowSize >>= \x -> return (getNewStartDialogue str y (getDir k e) x)
    msg = calculateMsgWinSize rules' p
    isInDialogue = if isNothing options && length str < x msg * y msg then MainGame else InDialogue
    basestate = State com game
    action = pos >>= \y -> updateWindow msgWin $ getWindowSize >>= \x -> drawClearMsg' x $ showDialogue d y (getDir k e) p

useInputKeyboardDead :: Common -> Game -> Event -> Point -> State
useInputKeyboardDead com@(Common _ mainWin msgWin _ _ k) game@(Game _ _ _ rules' d@(Dialogue str pos section options lastoption)) e p
  | e == exit k = basestate Nothing
  | otherwise = basestate $ Just $ drawClearMsg msgWin "You can only quit for now"
  where
    basestate = State com game Dead

runChoiceDialogue :: Common -> Game -> Event -> Point -> State
runChoiceDialogue com@(Common _ mainWin msgWin _ _ k) game@(Game _ _ _ rules' d@(Dialogue str pos section options lastoption)) e p
  | e == one k = run 1
  | e == two k = run 2
  | e == three k = run 3
  | e == four k = run 4
  | e == five k = run 5
  where
    run x' =useInputKeyboardD com (game {dialogue = newDialogue rules' (fst $ fromJust options !! (x'-1)) section False,
      rules = setOrUnsetLastoption rules' section lastoption }) (up k) p

updateScreenSize :: Common -> Game -> Point -> Curses ()
updateScreenSize (Common stdscr mainWin msgWin _ _ _ ) game y_x_width =  do
  updateWindow mainWin clear
  updateWindow msgWin clear
  let msdim = calculateMsgWinSize (rules game) y_x_width
  let mwdim = calculateMainWinSize (rules game) y_x_width
  updateWindow msgWin $ resizeWindow (toInteger $ y msdim) (toInteger $ x msdim)
  updateWindow mainWin $ resizeWindow (toInteger $ y mwdim) (toInteger $ x mwdim)
  updateBorders (rules game) stdscr y_x_width
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
testAndMoveC com game@(Game lm@(LevelMap _ currul) _ _ _ _ ) s winsize = State com game {m = lm {currul = if isOk then newul else currul}} MainGame action
  where
    newul@(Point ny nx) = currul + dirToPoint s
    isOk = isOnDisplayableMap lm newul && isOnDisplayableMap lm (newul + winsize + Point (-1) (-1))
    action = if isOk
      then Just $ updateCamera (mainWin com) (game {m = lm {currul = newul}}) >> drawClearMsg (msgWin com) "Camera moved"
      else Just $ drawClearMsg (msgWin com) "Could not move the camera"

-- Test and run the player move
testAndMoveP :: Common -> Game -> Direction -> Point -> State
testAndMoveP com@(Common stdscr mainWin msgWin _ _ k) game@(Game lm@(LevelMap map1 po) b _ rules _ ) s winsize = if isOk
  then testAndDoSomething (basestate $ Just $ updateCamera mainWin g>> drawClearMsg msgWin "Player moved") winsize
  else testAndDoSomething (basestate Nothing) winsize
  where
    newpos = pos b + dirToPoint s
    isOk = isOnDisplayableMap (LevelMap map1 po) newpos && canGoTrough lm newpos rules
    poskOkPlayer = if isOk then newpos else pos b
    newmap = moveCAtPos (y poskOkPlayer) (x poskOkPlayer) '@' $ removeFirstCharAt (y $ pos b) (x $ pos b)  map1
    g = game { player = b {pos=poskOkPlayer,look=s}, m = lm {levelMap = newmap}}
    basestate = State com g MainGame

-- Move the camera (do not do any test)
updateCamera :: Window -> Game -> Curses()
updateCamera win (Game (LevelMap map1 p ) b _ rules _) = getScreenSize >>= \x -> drawTab win (calculateMainWinSize rules x) (getCurrentDisplay actualmap p (calculateMainWinSize rules x))
  where
    radius = either (const 0) read $ get rules "GAME" "radius"
    actualmap = if 0 < radius
      then getRadius map1 rules (pos b) radius
      else map1

-- Test if can do something, and if possible actually do it
testAndDoSomething :: State -> Point -> State
testAndDoSomething (State com game@(Game lm@(LevelMap map1 _ ) p@(Beast pos dir _ _ _ _) _ rules _) status action) p' = case status of
  MainGame -> basestate $ if canInteractWith lm newpos rules "tosay" then Just $ action' >> drawClearMsg (msgWin com) (willDo' "tosay" "Would speak with") else cannot
  Action | canInteractWith lm newpos rules "dialogue" && not (isEnded rules section) -> useInputKeyboardD com (game {dialogue = newDialogue rules "dialogue" section True }) (up $ keyboard com) p'
    | canInteractWith lm newpos rules "hp" -> hitMonster com game newpos
    | otherwise -> basestate cannot
  where
    action' = fromMaybe (return ()) action
    newpos = pos + dirToPoint dir
    cannot = if isJust action then action else Just $ action' >> drawClearMsg (msgWin com) "Cannot do anything"
    willDo' =  willDo rules map1 newpos
    basestate = State com game MainGame
    section = getCellAt map1 newpos

hitMonster :: Common -> Game -> Point -> State
hitMonster com game@(Game lm@(LevelMap map1 _ ) p@(Beast _ dir _ dammage _ _) monsters' rules _) m_pos = State com newgame MainGame $ Just $ updateCamera (mainWin com) newgame >> drawClearMsg (msgWin com) msg
  where
    actual_monster = getBeast monsters' m_pos
    newmonster = maybe Nothing (\x -> Just x {hp= hp x - dammage}) actual_monster
    name' = maybe "noname" name actual_monster
    isDead x = hp x <= 0
    newmonsters =  maybe monsters' (\x -> fromJust newmonster : delete x monsters') actual_monster
    newmap = maybe map1 (\x -> if isDead x then removeDead map1 [x] else map1) newmonster
    msg = maybe "error" (\x -> if isDead x then name' ++ " is dead" else name' ++ " was hit") newmonster
    newgame = game { m = lm {levelMap = newmap}, monsters = newmonsters }

-- Return the player after it was hit, and the number of hit
todoMonsters :: Common -> Game -> Monsters -> Int -> (Beast, Int)
todoMonsters _ g [] b = (player g, b)
todoMonsters com game@(Game lm@(LevelMap map1 _ ) p@(Beast pos' _ hp' _ _ _) _ rules _) (x:xs) b = todoMonsters com newgame xs $ b + bobo
  where
    isOk = isNear pos' $ pos x
    bobo = if isOk then 1 else 0
    newgame = game {player = if isOk then p {hp = hp' - dammage x} else p}
