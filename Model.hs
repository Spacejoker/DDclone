module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

height :: Int
height = 19

width :: Int
width = 19

type Coord = (Int, Int)

data Player = Player {
  pPos :: Coord,
  pHealth :: Int,
  pMaxHealth :: Int,
  pDmg :: Int
} deriving Eq

data Enemy = Enemy {
  ePos :: Coord,
  eHealth :: Int,
  eMaxHealth :: Int
} deriving (Show, Eq)

data Graphics = Graphics {
  test :: Surface,
  playerSurface :: Surface,
  enemySurface :: Surface,
  floorSurface :: Surface,
  wallSurface :: Surface,
  font :: Font
}

data GameState = GameState {
  running :: Bool,
  clickpos :: [Coord],
  fov :: [(Int, Int, Int)],
  pf :: [(Int, Int, Int)],
  enemies :: [Enemy],
  gPlayer :: Player,
  gEnemyMouse :: Int
}

