module ConvertGrid where

import Model
import Debug.Trace

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
        (a, x:b)  = trace (show enemyIdx) (splitAt enemyIdx (enemies gs))
        newEnemy = applyPlayerDamage (gPlayer gs) x
        

applyPlayerDamage :: Player -> Enemy -> Enemy
applyPlayerDamage player e = e { eHealth = eHealth e - (pDmg player) }

--
--gPlayer = modPlayer, 
        -- player = trace (show enemyIdx) $ gPlayer gs
        --newHp = (pHealth player) - 1
        --modPlayer = player { pHealth = newHp }
