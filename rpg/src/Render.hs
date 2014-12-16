module Render where 
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Dungeon

renderWorld :: GameState -> IO ()
renderWorld w = do
  s <- getVideoSurface 
  renderLayer s 0 (gArea w) (gGx w)
  SDL.flip s 
  putStr ""

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
