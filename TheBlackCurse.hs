import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import System.Exit

main :: IO ()
main = do
  initScr --Start
  echo False -- Disable echo

  let msgwin_width = 5
  x_y_width <- scrSize
  mainwin <- newWin ((fst x_y_width) - msgwin_width) 0 msgwin_width 0
  msgwin <- newWin msgwin_width 0 0 0

  wMove mainwin 1 1
  wMove msgwin 1 1

  mainLoop mainwin msgwin

mainLoop :: Window -> Window -> IO ()
mainLoop mainwin msgwin = do
  inp <- getCh

  werase msgwin
  werase mainwin

  useInput msgwin inp

  refreshWindow mainwin
  refreshWindow msgwin

  mainLoop mainwin msgwin

useInput :: Window -> Key -> IO()
useInput msgwin s
  | s == KeyChar '\ESC' = (endWin >> exitSuccess) -- Exit when ESC is pressed
  | elem (head (displayKey s)) ['q','s','z','d'] = mvWAddStr msgwin 1 1 "A direction was pressed" -- moveCharacter s
  | otherwise = mvWAddStr msgwin 1 1 $ displayKey s

refreshWindow :: Window -> IO()
refreshWindow win = do
  wBorder win defaultBorder
  wRefresh win
