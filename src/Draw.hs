module Draw
  (drawClearMsg,
  drawTab,
  makeBorders,
  getCurrentDisplay)

where

import UI.NCurses
import Data.List

-- Draw a message on the window if possible (clear all before)
drawClearMsg :: Window -> String -> Curses ()
drawClearMsg win str = updateWindow win $ do
  y_x_width <- windowSize
  clear
  drawString $ if ((fromIntegral ((fst y_x_width)*(snd y_x_width))) > (length str)) then str else "Msg too big"

-- Draw a tab of String
drawTab :: Window -> [String] -> Curses ()
drawTab win tab = drawClearMsg win $ init (concat tab)

-- Draw borders of a rectangle starting at (pos_x,pos_y) with y rows and x columns on win
makeBorders :: Window -> (Integer,Integer) -> Integer -> Integer -> Curses ()
makeBorders win (pos_y,pos_x) y x = updateWindow win $ do
  moveCursor pos_y pos_x
  drawGlyph glyphCornerUL
  drawLineH (Just glyphLineH) $ x-2 -- Horizontal top line
  moveCursor (pos_y+1) pos_x
  drawLineV (Just glyphLineV) $ y-2 -- Vertical left line
  moveCursor (pos_y+y-1) pos_x
  drawGlyph glyphCornerLL
  moveCursor (pos_y+y-1) (pos_x+1)
  drawLineH (Just glyphLineH) $ x-2 -- Horizontal bottom line
  moveCursor (pos_y+y-1) (pos_x+x-1)
  drawLineH (Just glyphCornerLR) 1 -- drawGlyph move the cursor, we don't want it to go out of the window; drawlineH don't
  moveCursor (pos_y) (pos_x+x-1)
  drawGlyph glyphCornerUR
  moveCursor (pos_y+1) (pos_x+x-1)
  drawLineV (Just glyphLineV) $ y-2 -- Vertical Right line
  moveCursor pos_y pos_x -- Reset cursor pos

-- Reduce if possible the map to a map of (height,width) starting at (starty,startx)
getCurrentDisplay :: [[Char]] -> (Integer,Integer) -> (Integer,Integer) -> [[Char]]
getCurrentDisplay tab (starty,startx) (height,width) = take (fromIntegral height) $ map (take (fromIntegral width)) $ drop (fromIntegral starty) $ map (drop (fromIntegral startx)) tab
