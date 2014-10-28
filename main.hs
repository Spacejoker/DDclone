import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi
import Data.List
import Debug.Trace

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
  fov :: [(Int, Int, Int)],
  pf :: [(Int, Int, Int)],
  enemies :: [(Int, Int, Int)]
}

main :: IO()
main = do
  SDL.init [InitEverything]
  setVideoMode 800 608 32 []
  TTF.init

  setCaption "A" "B" 

  enableKeyRepeat 500 30

  char <- SDLi.load "image/rpg_sprites_10.PNG"

  let b = [(x, y, if x == 1 && y ==1 then 1 else 0) | x <- [0..width-1], y <- [0..height-1]]
  let pf' = [(x,y,z) | x <- [0..width-1], y <- [0..height-1], z <- [0,1], z == (x+y) `mod` 2]

  gameLoop $ GameState char True [(10, 10)] b pf' [(0,0,1)]

gameLoop :: GameState -> IO ()
gameLoop gs = do

  s <- getVideoSurface

  gs' <- tickGame gs
  SDL.flip s

  let blit = (\(a, b) -> blitSurface (image gs) (Just $ Rect 70 70 32 32) s (Just (Rect a b 0 0)))
  mapM_ blit (clickpos gs)

  fillRect s (Just $ Rect 0 0 800 608) (Pixel 0)
  
  let explored_pf = filter (\(x, y, _) -> (x,y,1) `elem` (fov gs)) (pf gs)
  let fillVal = (\(x, y, val) -> fillRect s (Just $ Rect (x*32) (y*32) 32 32) (Pixel (100*(1+(fromIntegral val)))))
  mapM_ fillVal explored_pf

  case running gs of
    True -> gameLoop gs'
    _ -> return()

tickGame :: GameState -> IO GameState
tickGame gs = do
  events <- getEvents pollEvent []
  let ret = foldl handleEvent gs events
  return ret

--Explore map
handleClick :: GameState -> Coord -> GameState
handleClick gs (mx, my)
  | notFree = trace "NOT" gs -- not explored yet, nothing happens
  | hasEnemy = trace "Enemy" gs
  | otherwise = gs {fov = fov'}
  where notFree = (mx, my, 0) `elem` (fov gs)
        hasEnemy = length (filter (\(x,y,_) -> x == mx && y == my) (enemies gs)) > 0
        fov' = map (u (mx, my)) (fov gs)
        (_,_,zz) = head $ filter (\(x',y',_) -> x' == mx && y' == my) (fov gs)
        (_,_,pfv) = head $ filter (\(x,y,_) -> x == mx && y == my) (pf gs)
        u = (\(x, y) (x', y', val) -> 
          if zz == 1 && 
              abs (x' - x) <= 1 && 
              abs (y' - y) <= 1 &&
              pfv == 0
            then (x', y', 1) 
            else (x', y', val))

handleEvent :: GameState -> Event -> GameState
handleEvent gs e =
  case e of
    KeyDown (Keysym SDLK_SPACE _ _) -> gs {running = False}
    MouseButtonDown x y ButtonLeft  -> 
      handleClick gs (((fromIntegral x) `quot` 32), ((fromIntegral y) `quot` 32)) 
    _                               -> gs
    
-- stolen code from mr cadr, works nice
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

