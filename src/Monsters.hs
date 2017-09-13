module Monsters
    ( Monsters (..),
    removeDead,
    findActivated,
    findMonsters,
    getBeast,
    hitMonster,
    todoMonsters,
    moveMonsters
    ) where

import Data.List
import Data.ConfigFile
import Data.Maybe
import Control.Monad (when)

import Space
import Beast
import LevelMap
import GameTypes
import Draw

findMonsters :: LevelMap -> ConfigParser -> Monsters
findMonsters (LevelMap lm _) cp = findMonsters' lm cp $ findMonstersInCp cp $ sections cp

findMonsters' :: Map -> ConfigParser -> [SectionSpec] -> Monsters
findMonsters' _ _ [] = []
findMonsters' lm cp (x:xs) = Beast (getStrPos lm x 0 0) DOWN (read $ findWithPrefix cp "hp" x "0") (read $ findWithPrefix cp "dammage" x "0") (read $ findWithPrefix cp "activated" x "0") (findWithPrefix cp "name" x "no name") : findMonsters' lm cp xs

findMonstersInCp :: ConfigParser -> [SectionSpec]-> [SectionSpec]
findMonstersInCp _ [] = []
findMonstersInCp cp (x:xs) = if has_option cp x "monster" && x /= "PLAYER" then x : findMonstersInCp cp xs else findMonstersInCp cp xs

findActivated :: Point -> ConfigParser -> Monsters -> Monsters
findActivated _ _ [] = []
findActivated charpos cp (x:xs) = if dist (pos x) charpos <= activated x then x : todo else todo
  where
    todo = findActivated charpos cp xs

removeDead :: Map -> Monsters -> Map
removeDead m [] = m
removeDead m (Beast p@(Point y x) _ _ _ _ _:xs) = replaceByStr (removeDead m xs) y x [last $ getCellAt m p]

getBeast :: Monsters -> Point -> Maybe Beast
getBeast [] _ = Nothing
getBeast (x:xs) p
  | pos x == p = Just x
  | otherwise = getBeast xs p

hitMonster :: Common -> Game -> Point -> State
hitMonster com game@(Game lm@(LevelMap map1 _ ) p@(Beast _ dir _ dammage _ _) monsters' rules _) m_pos = State com newgame MainGame $ updateCamera (mainWin com) newgame >> drawClearMsg (msgWin com) msg
  where
    actual_monster = getBeast monsters' m_pos
    newmonster = maybe Nothing (\x -> Just x {hp= hp x - dammage}) actual_monster
    name' = maybe "noname" name actual_monster
    isDead x = hp x <= 0
    newmonsters = maybe monsters' (\x -> if isDead (fromJust newmonster) then delete x monsters' else fromJust newmonster : delete x monsters') actual_monster
    newmap = maybe map1 (\x -> if isDead x then removeDead map1 [x] else map1) newmonster
    msg = maybe "error" (\x -> if isDead x then name' ++ " is dead" else name' ++ " was hit. It have now " ++ show (hp x) ++ "hp") newmonster
    newgame = game { m = lm {levelMap = newmap}, monsters = newmonsters }

-- Return the player after it was hit, and the number of hit
todoMonsters :: State -> State
todoMonsters s@(State _ (Game _ _ [] _ _) _ _) = s
todoMonsters (State com game'@(Game lm@(LevelMap map1 _ ) p@(Beast pos' _ hp' _ _ _) monsters' rules' _) status todo') = State com newgame newstatus newtodo
  where
    activated = findActivated pos' rules' monsters'
    (newmap, newmonsters) = moveMonsters map1 pos' monsters' activated rules'
    (newplayer,bobo) = todoMonsters' com game' activated False
    newgame = game' {player = newplayer, m = lm {levelMap=newmap}, monsters = newmonsters}
    newtodo = todo' >>  when (newmonsters /= monsters') (updateCamera (mainWin com) newgame)
      >> when bobo (appendMsg (msgWin com) $ "\nYou were hit, you have now " ++ show (hp newplayer) ++ " hp" )
      >> when isDead (drawClearMsg (msgWin com) "You are dead" )
    isDead = hp newplayer <= 0
    newstatus = if isDead then Dead else status

todoMonsters' :: Common -> Game -> Monsters -> Bool -> (Beast, Bool)
todoMonsters' _ g [] b = (player g, b)
todoMonsters' com game@(Game lm@(LevelMap map1 _ ) p@(Beast pos' _ hp' _ _ _) _ rules _) (x:xs) b = todoMonsters' com newgame xs $ isOk || b
  where
    isOk = isNear pos' $ pos x
    newgame = if isOk then game {player = p {hp = hp' - dammage x}} else game

-- TODO: don't superopose monsters
moveMonsters :: Map -> Point -> Monsters -> Monsters -> ConfigParser ->(Map,Monsters)
moveMonsters m _ monst [] _ = (m,monst)
moveMonsters m charpos monst (am@(Beast curr _ _ _ _ n):xs) rules = moveMonsters newmap charpos newmonst xs rules
  where
    isOk = isNear curr charpos
    fstnewpos = curr + signumFst (charpos - curr)
    newpos@(Point y' x') = if canGoTrough (LevelMap m (Point 0 0)) fstnewpos rules then fstnewpos else curr
    newmap = if isOk then m else moveCAtPos y' x' (head n) $ replaceByStr m (y curr) (x curr) [last $ getCellAt m curr]
    pos = fromJust $ elemIndex am monst
    newmonst = if isOk then monst else take pos monst ++ [am {pos=newpos}] ++ drop (pos+1) monst
