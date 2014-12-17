
module Dungeon where
import Data.Functor
import Graphics.UI.SDL as SDL
import Debug.Trace

type Coord = (Int, Int)

data Player = Player {
  pPos :: Coord,
  pFacing :: Direction
} deriving (Eq, Show)

data GroundType = Wall | Floor | Door
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

data NpcType = Shopkeeper | Unknown
  deriving Show

data Npc = Npc {
  nPos :: Coord,
  npcType :: NpcType
} deriving Show

data Area = Area {
  aBoard :: [(Coord, GroundType)],
  aNpcs :: [Npc],
  aTransitions :: [(Coord, Int)],
  aEncounterRate :: Maybe (Int, Int)
} deriving Show

data GameState = GameState {
  gRunning :: Bool,
  gPlayer :: Player,
  gArea :: Area,
  gGx :: [Surface]
} deriving Show

areaTransitions :: GameState -> [(Coord, Int)]
areaTransitions = aTransitions . gArea

playerPos :: GameState -> Coord
playerPos = pPos . gPlayer

screenTransition :: GameState -> Bool
screenTransition gs = (playerPos gs) `elem` (map fst (areaTransitions gs))

transition ::GameState -> GameState
transition gs = gs { gArea = areas !! nextArea }
  where nextArea = snd $ head $ filter (\(pos, _) -> pos == playerPos gs) (areaTransitions gs)

dungeonMap = 
  ["################"
  ,"#..............#"
  ,"#>.............#"
  ,"#..............#"
  ,"#..............#"
  ,"#........###...#"
  ,"#........###...#"
  ,"#..............#"
  ,"#..............#"
  ,"################"
  ]

worldMap = 
  ["##################"
  ,"##################"
  ,"#...............##"
  ,"#...>#############"
  ,"#...............##"
  ,"#...............##"
  ,"#....######.....##"
  ,"#....#..........##"
  ,"#....#..........##"
  ,"##################"
  ]

areas = [dungeon, world]

charToTile :: Char -> GroundType
charToTile c = case c of
  '#' -> Wall
  '.' -> Floor
  '>' -> Door

dungeon :: Area
dungeon = Area (toCoordAndType dungeonMap) [Npc (2,2) Shopkeeper] [((1,2), 1)] Nothing

world :: Area
world = Area (toCoordAndType worldMap) [] [((4,3), 0)] (Just (3, 8))

toCoordAndType :: [[Char]] -> [(Coord, GroundType)]
toCoordAndType rows = ret
  where xNum = map (zip [0..]) rows
        yNum = zip [0..] xNum 
        f = \(x, list) -> map (\val -> (x, val)) list
        ret = map (\(y, (x, c)) -> ((x, y), (charToTile c))) $ concat $ map f yNum

newPlayer :: Player
newPlayer = Player (1,1) South

makeNewGame :: [Surface] -> GameState
makeNewGame s = GameState True newPlayer dungeon s
