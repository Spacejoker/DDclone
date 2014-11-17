module GameLogic where

import Model

movePlayer :: Player -> Coord -> Player
movePlayer p newPos = p {pPos = newPos}


levelPlayer :: Player -> Player
levelPlayer p = p { pLevelLimit = newLevelLimit, pExp = newExp, pLevel = newLevel, pMaxHealth = newMaxHp , pHealth = newMaxHp}
  where newExp = (pExp p) - (pLevelLimit p)
        newMaxHp = (pMaxHealth p) + 10
        newLevelLimit = (pLevelLimit p) + 10
        newLevel = (pLevel p) + 1

handleLevelUp :: GameState -> GameState
handleLevelUp gs
  | pExp p >= pLevelLimit p = handleLevelUp gs { gPlayer = levelPlayer $ gPlayer gs }
  | otherwise = gs
    where p = gPlayer gs

checkEnemiesLeft ::GameState -> GameState
checkEnemiesLeft gs
  | (length $ enemies gs) == 0 = gs {gWin = True }
  | otherwise = gs

