module ConvertGrid where

import Model
import Data.List

toGrid :: Coord -> Coord
toGrid (x, y) = (x `quot` 32, y `quot` 32)

findEnemy :: Coord -> [Enemy] -> Int
findEnemy pos e = findEnemyZip pos (zip [0,1..] e)

findEnemyZip :: Coord -> [(Int, Enemy)] -> Int
findEnemyZip _ [] = (-1)
findEnemyZip pos ((idx, e):es) 
  | ePos e == pos = idx
  | otherwise = findEnemyZip pos es

handlePlayerAttack :: Coord -> GameState -> GameState
handlePlayerAttack pos gs = gs { enemies = a ++ [newEnemy] ++ b }
  where enemyIdx = findEnemy pos (enemies gs)
        (a, x:b)  = (splitAt enemyIdx (enemies gs))
        newEnemy = applyPlayerDamage (gPlayer gs) x

applyPlayerDamage :: Player -> Enemy -> Enemy
applyPlayerDamage player e = e { eHealth = eHealth e - (pDmg player) }

killDeadEnemies :: GameState -> GameState
killDeadEnemies gs = gs { enemies = alive, gPlayer = p' }
  where (dead, alive) = partition (\x -> eHealth x <= 0) (enemies gs)
        sumExp = (sum . map (\x -> eExpWorth x)) dead
        p = gPlayer gs
        p' = p { pExp = (pExp p) + sumExp }
