import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

-- import System.Random

height :: Int
height = 19

width :: Int
width = 19

type Coord = (Int, Int)

data GameState = GameState {
  image :: Surface,
  running :: Bool,
  clickpos :: [Coord],
  board :: [(Int, Int, Int)]
}

main :: IO()
main = do
  SDL.init [InitEverything]
  setVideoMode 800 608 32 []
  TTF.init

  setCaption "A" "B" 

  enableKeyRepeat 500 30

  char <- SDLi.load "image/rpg_sprites_10.PNG"

  let b = [(x, y, val) | x <- [0..width-1], y <- [0..height-1], val <-[1,2] , val == ((y+x) `mod` 2)]

  gameLoop $ GameState char True [(10, 10)] b

gameLoop :: GameState -> IO ()
gameLoop gs = do

  s <- getVideoSurface

  gs' <- tickGame gs
  SDL.flip s

  let blit = (\(a, b) -> blitSurface (image gs) (Just $ Rect 70 70 32 32) s (Just (Rect a b 0 0)))
  mapM_ blit (clickpos gs)

  let fillVal = (\(x, y, val) -> fillRect s (Just $ Rect (x*32) (y*32) 32 32) (Pixel (255*(fromIntegral val))))
  mapM_ fillVal (board gs)

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

