import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events as SDLe
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

-- import System.Random

type Coord = (Int, Int)

data GameState = GameState {
  image :: Surface,
  running :: Bool,
  clickpos :: [Coord]
}

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "A" "B" 

  enableKeyRepeat 500 30

  char <- SDLi.load "image/rpg_sprites_10.PNG"

  gameLoop $ GameState char True [(10, 10)]

gameLoop :: GameState -> IO ()
gameLoop gs = do

  s <- getVideoSurface

  gs' <- tickGame gs
  SDL.flip s

  let blit = (\(a, b) -> blitSurface (image gs) (Just $ Rect 70 70 32 32) s (Just (Rect a b 0 0)))
  mapM_ blit (clickpos gs)

  case running gs of
    True -> gameLoop gs'
    _ -> return()

tickGame :: GameState -> IO GameState
tickGame gs = do
  events <- getEvents pollEvent []
  let ret = foldl handleEvent gs events
  return ret

handleEvent :: GameState -> Event -> GameState
handleEvent gs e =
  case e of
    KeyDown (Keysym SDLK_SPACE _ _) -> gs {running = False}
    MouseButtonDown x y ButtonLeft -> gs { 
      clickpos = ((fromIntegral x, fromIntegral y):clickpos gs) 
    }
    _ -> gs
    
-- stolen code from mr cadr, works nice
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)
