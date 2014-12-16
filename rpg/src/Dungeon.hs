
module Dungeon where
import Data.Functor
import Graphics.UI.SDL as SDL

type Coord = (Int, Int)

data Player = Player {
  pPos :: Coord,
  pHealth :: Int,
  pMaxHealth :: Int,
  pDmg :: Int
} deriving (Eq, Show)

data GroundType = Wall | Floor 
  deriving (Show, Eq)

--data Position p = At p | Undefined
  --deriving (Eq, Show)

--instance Functor Position where
  --fmap _ Undefined     = Undefined
  --fmap f (At p)        = At (f p)

data Direction = North | South | West | East
  deriving (Show, Eq)

data Gx = Gx {
  grassTile :: Surface
}
data Area = Area {
  gBoard :: [(Coord, GroundType)]
} deriving Show

data GameState = GameState {
  gRunning :: Bool,
  gPlayer :: Player,
  gArea :: Area,
  gGx :: [Surface]
} deriving Show

dungeonMap = 
  ["################"
  ,"#..............#"
  ,"#..............#"
  ,"#..............#"
  ,"#..............#"
  ,"#........###...#"
  ,"#........###...#"
  ,"#..............#"
  ,"#..............#"
  ,"################"
  ]

charToGroundType :: Char -> GroundType
charToGroundType c'= case c' of
  '#' -> Wall
  '.' -> Floor

dungeon :: Area
dungeon = Area $ toCoordAndType dungeonMap

toCoordAndType :: [[Char]] -> [(Coord, GroundType)]
toCoordAndType rows = ret
  where xNum = map (zip [0..]) rows
        yNum = zip [0..] xNum 
        f = \(x, list) -> map (\val -> (x, val)) list
        ret = map (\(y, (x, c)) -> ((x, y), (charToTile c))) $ concat $ map f yNum

charToTile :: Char -> GroundType
charToTile '#' = Wall
charToTile _   = Floor

newPlayer :: Player
newPlayer = Player (1,1) 10 10 1

makeNewGame :: [Surface] -> GameState
makeNewGame s = GameState True newPlayer dungeon s

walk :: Direction -> Coord -> Coord
walk East (x, y) = (y, x+1)
