
module Dungeon where
import Data.Functor

type Coord = (Int, Int)

data Player = Player {
  pPos :: Position Coord,
  pHealth :: Int,
  pMaxHealth :: Int,
  pDmg :: Int
} deriving (Eq, Show)

data GroundType = Wall | Floor 
  deriving (Show, Eq)

data Position p = At p | Undefined
  deriving (Eq, Show)

instance Functor Position where
  fmap _ Undefined     = Undefined
  fmap f (At p)        = At (f p)

data Direction = North | South | West | East
  deriving (Show, Eq)

data Area = Area {
  gBoard :: [(Coord, GroundType)]
} deriving Show

data GameState = GameState {
  gRunning :: Bool,
  gPlayer :: Player,
  gArea :: Area
} deriving Show

dungeonMap = 
  ["#####"
  ,"#...#"
  ,"#...#"
  ,"#...#"
  ,"#####"
  ]


charToGroundType :: Char -> GroundType
charToGroundType c'= case c' of
  '#' -> Wall
  '.' -> Floor

dungeon :: Area
dungeon = Area $ map (\(y, (x, c)) -> ((y, x), charToGroundType c)) zipped
  where xzip = concat $ map (zip [0..]) dungeonMap
        zipped = zip [0..] xzip

newPlayer :: Player
newPlayer = Player (At (1,1)) 10 10 1

makeNewGame :: GameState
makeNewGame = GameState True newPlayer dungeon

walk :: Direction -> Coord -> Coord
walk East (x, y) = (y, x+1)
