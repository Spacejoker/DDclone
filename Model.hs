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
}

data Enemy = Enemy {
  ePos :: Coord,
  eHealth :: Int,
  eMaxHealth :: Int
}

data Graphics = Graphics {
  test :: Surface,
  playerSurface :: Surface,
  enemySurface :: Surface,
  floorSurface :: Surface,
  wallSurface :: Surface
}

data GameState = GameState {
  graphics :: Graphics,
  running :: Bool,
  clickpos :: [Coord],
  fov :: [(Int, Int, Int)],
  pf :: [(Int, Int, Int)],
  enemies :: [Enemy],
  gPlayer :: Player,
  font :: Font,
  gEnemyMouse :: Int
}

