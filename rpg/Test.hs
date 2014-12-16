module TestMain where 
import Test.HUnit
import Dungeon


basic :: Test
basic = TestList
  [ "Map to [(Coord, GroundType)]" ~: 25 ~=? length x
  , "Correct head on gBoard dungeon" ~: ((0,0), Wall) ~=? h
  ]
    where x = gBoard dungeon
          h = head x

playerMovement :: Test
playerMovement = TestList
  [ "Right wall detection" ~: At (1,3) ~=? p ]
    where g0 = makeNewGame
          actions = [walk East, walk East]
          g1 = foldr walk makeNewGame actions
          p = pPos $ gPlayer g1



main :: IO Counts
main = runTestTT $ TestList 
  [ basic,
    playerMovement
  ]
