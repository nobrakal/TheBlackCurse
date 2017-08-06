import UI.HSCurses.Curses
import System.Exit

main :: IO ()
main = do
  win <- initScr --Start
  echo False
  wAddStr win $ "Salut ça va \n"
  refresh

  mainLoop win

  endWin -- Stop
  putStrLn "FIN"

mainLoop :: Window -> IO ()
mainLoop win = do
  inp <- getCh
  useInput win inp
  refresh
  mainLoop win

useInput :: Window -> Key -> IO()
useInput win s
  | s == KeyChar 'q' = (endWin >> exitSuccess)
  | otherwise = wAddStr win $ (show s) ++ "\n"
