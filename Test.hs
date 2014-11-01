module TestMain where 
import Test.HUnit
import ConvertGrid (toGrid, findEnemy, applyPlayerDamage, handlePlayerAttack)
import Model


enemyA :: Enemy
enemyA = Enemy (0,0) 5 5 
enemyB :: Enemy
enemyB = Enemy (1,1) 5 5 
player :: Player
player = Player (2,2) 5 5 1

testGs :: GameState
testGs = GameState True [] [] [] [enemyA, enemyB] player  0

testToGrid :: Test
testToGrid = TestList
  [ "Basic Grid test #1" ~: toGrid (31, 31) ~=? (0,0) 
  , "Basic Grid test #2" ~: toGrid (32, 32) ~=? (1,1) 
  ]

testFindEnemy :: Test
testFindEnemy = TestList
  [ "Find second enemy" ~: findEnemy (1,1) [Enemy (0,0) 0 0, Enemy(1,1) 0 0] ~=? 1
  , "Find first enemy" ~: findEnemy (0,0) [Enemy (0,0) 0 0, Enemy(1,1) 0 0] ~=? 0
  , "Find no enemy" ~: findEnemy (2,2) [Enemy (0,0) 0 0, Enemy(1,1) 0 0] ~=? (-1)
  ]

testPlayerAttack :: Test
testPlayerAttack = TestList
  [ "Attack chosen enemy" ~: applyPlayerDamage player enemyA ~=? Enemy (0,0) 4 5
  , "Attack enemy in array" ~: enemies (handlePlayerAttack (1,1) testGs) ~=? 
      [enemyA, enemyB { eHealth = 4}]
  ]

main :: IO Counts
main = runTestTT $ TestList 
  [ testToGrid
  , testFindEnemy
  , testPlayerAttack
  ]
