module Render where 
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Dungeon

renderWorld :: GameState -> IO ()
renderWorld gs = do
  s <- getVideoSurface 
  renderLayer s 0 (gArea gs) (gGx gs)
  renderPlayer s (gPlayer gs) (gGx gs)
  SDL.flip s 
  putStr ""

renderPlayer :: Surface -> Player -> [Surface]-> IO(Bool) 
renderPlayer s p (f:fs) = 
  blitSurface 
    f
    (Just $ Rect 32 32 32 32)
    s
    (Just $ getPlayerDispRect p)

getPlayerDispRect :: Player -> Rect
getPlayerDispRect p = Rect ((fst pos)*32) ((snd pos)*32) 32 32
  where pos = pPos p

renderLayer :: Surface -> Int -> Area -> [Surface] -> IO()
renderLayer s _ area gx = do
  let board = gBoard area
  let dim = (40::Int)
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
