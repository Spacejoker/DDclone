module Render where 
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Dungeon

renderWorld :: GameState -> IO ()
renderWorld gs = do
  s <- getVideoSurface 
  fillRect s (Just $ Rect 0 0 800 600) (Pixel 0) 
  renderLayer s 0 (gArea gs) (gGx gs)
  renderPlayer s (gPlayer gs) (gGx gs)
  renderNpcs s (gArea gs) (gGx gs)
  SDL.flip s 
  putStr ""


renderPlayer :: Surface -> Player -> [Surface]-> IO(Bool) 
renderPlayer s p ss = 
  blitSurface 
    (playerSurface p ss)
    (Just $ playerRect p)
    s
    (Just $ getPlayerDispRect p)

playerSurface :: Player -> [Surface] -> Surface
playerSurface _ surfaces = surfaces !! 1

playerRect :: Player -> Rect
playerRect p
 | face == North = Rect 0 (48*3) 32 48
 | face == South = Rect 0 0 32 48
 | face == West = Rect 0 (48) 32 48
 | face == East = Rect 0 (48*3) 32 48
   where face = pFacing p
 

getPlayerDispRect :: Player -> Rect
getPlayerDispRect p = Rect ((fst pos)*32) ((snd pos)*32 - 16) 32 48
  where pos = pPos p

renderNpcs :: Surface -> Area -> [Surface] -> IO()
renderNpcs s area (f:fs) = 
  mapM_ blitNpc (aNpcs area)
    where blitNpc npc = blitSurface 
            f
            (Just $ Rect 64 64 32 32)
            s 
            (Just $ Rect (y*dim) (x*dim) dim dim) 
              where (x, y) = nPos npc
          dim = 32

renderLayer :: Surface -> Int -> Area -> [Surface] -> IO()
renderLayer s _ area gx = do
  let board = aBoard area
  mapM_ (blitTile gx s) board

blitTile :: [Surface] -> Surface -> ((Int, Int), GroundType) -> IO(Bool)
blitTile (f:fs) s ((y, x), t) = 
  blitSurface 
    f
    (Just $ rectFromGroundType t) 
    s 
    (Just $ Rect (y*dim) (x*dim) dim dim) 
      where  dim = 32

rectFromGroundType :: GroundType -> Rect
rectFromGroundType Wall = Rect 0 0 32 32
rectFromGroundType Floor = Rect 0 (11*32) 32 32
rectFromGroundType Door = Rect 0 (2*32) 32 32
