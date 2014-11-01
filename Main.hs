module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Debug.Trace

import Model
import ConvertGrid (toGrid, findEnemy, handlePlayerAttack)

main :: IO()
main = do
  SDL.init [InitEverything]
  setVideoMode 800 608 32 []
  TTF.init

  setCaption "A" "B" 

  enableKeyRepeat 500 30

  fnt <- openFont "font.ttf" 30
  char <- SDLi.load "image/rpg_sprites_10.PNG"
  mainchar <- SDLi.load "image/char.png"
  mob <- SDLi.load "image/enemy.png"
  floor_ <- SDLi.load "image/floor.png"
  wall <- SDLi.load "image/wall.png"

  let fov' = [(x, y, if x <= 2 && y <= 2 then 1 else 0) | x <- [0..width-1], y <- [0..height-1]]
  let pf' = [(x,y,z) | x <- [0..width-1], y <- [0..height-1], z <- [0,1], z == (x+y) `mod` 2]

  let gs = Graphics char mainchar mob floor_ wall
  let p = Player (1,1) 10 10 2
  let testEnemies = [Enemy (0,0) 10 10, Enemy (3,3) 15 15, Enemy (1,3) 20 20]

  gameLoop $ GameState gs True [(10, 10)] fov' pf' testEnemies p fnt (0)

drawSprite :: Surface -> Surface -> Coord -> IO(Bool)
drawSprite sprite dest (x, y) = blitSurface sprite Nothing dest (Just $ Rect (x*32) (y*32) 32 32 )

hpString :: Player -> String
hpString p = (show $ pHealth p) ++ " / " ++ (show $ pMaxHealth p)

enemyHpString :: Enemy -> String
enemyHpString e = (show $ eHealth e) ++ "/" ++ (show $ eMaxHealth e)

drawInfo :: Int -> Surface -> GameState -> IO(Bool)
drawInfo (-1) _ _ = return (True)
drawInfo idx s gs= do
  strToBlit <- renderTextSolid (font gs) (enemyHpString $ (enemies gs) !! idx) (Color 255 0 0)
  blitSurface strToBlit Nothing s (Just $ Rect 500 400 0 0)
  

gameLoop :: GameState -> IO ()
gameLoop gs = do

  s <- getVideoSurface
  gs' <- tickGame gs

  --reset screen
  fillRect s (Just $ Rect 0 0 800 608) (Pixel 0)
  
  let explored_pf = filter (\(x, y, _) -> (x,y,1) `elem` (fov gs)) (pf gs)

  let fillVal = (\(x, y, val) -> fillRect s (Just $ Rect (x*32) (y*32) 32 32) (Pixel (100*(1+(fromIntegral val)))))

  mapM_ fillVal explored_pf

  drawSprite (playerSurface $ graphics gs) s (pPos $ gPlayer gs)

  let enemySprite = enemySurface $ graphics gs
  mapM_ (\e -> drawSprite enemySprite s (ePos e)) (enemies gs)

  title <- renderTextSolid (font gs) (hpString $ gPlayer gs) (Color 255 0 0)
  blitSurface title Nothing s (Just $ Rect 500 500 0 0)

  let enemyIdx = gEnemyMouse gs

  -- Draw info panel
  drawInfo enemyIdx s gs

  SDL.flip s

  case running gs of
    True -> gameLoop gs'
    _ -> return()

tickGame :: GameState -> IO GameState
tickGame gs = do
  events <- getEvents pollEvent []
  let ret = foldl handleEvent gs events
  return ret

movePlayer :: Player -> Coord -> Player
movePlayer p newPos = p {pPos = newPos}

valueOf ::  (Int, Int) -> [(Int, Int, a)] -> (Int, Int, a)
valueOf (mx, my) list = head $ filter (\(x',y',_) -> x' == mx && y' == my) list




--Explore map
handleClick :: GameState -> Coord -> GameState
handleClick gs (mx, my)
  | notFree = trace "NOT" gs
  | hasEnemy = trace "Enemy" (handlePlayerAttack (mx, my) gs)
  | hasWall = trace "Wall" gs
  | otherwise = gs {fov = fov', gPlayer = movePlayer (gPlayer gs) (mx, my)}
  where notFree = (mx, my, 0) `elem` (fov gs)
        hasEnemy = length (filter (\e -> (fst $ ePos e) == mx && (snd $ ePos e) == my) (enemies gs)) > 0
        hasWall = pfv /= 0
        fov' = map (u (mx, my)) (fov gs)
        (_,_,zz) = valueOf (mx, my) (fov gs) 
        (_,_,pfv) = valueOf (mx, my) (pf gs)
        u = (\(x, y) (x', y', val) -> 
          if zz == 1 && 
              abs (x' - x) <= 1 && 
              abs (y' - y) <= 1 &&
              pfv == 0
            then (x', y', 1) 
            else (x', y', val))

handleMouseOver :: GameState -> (Int, Int) -> GameState
handleMouseOver gs pos = gs {gEnemyMouse = enemyIdx }
  where enemyIdx = findEnemy (toGrid pos) (enemies gs)

handleEvent :: GameState -> Event -> GameState
handleEvent gs e =
  case e of
    KeyDown (Keysym SDLK_SPACE _ _) -> gs {running = False}
    MouseButtonDown x y ButtonLeft  -> 
      handleClick gs (((fromIntegral x) `quot` 32), ((fromIntegral y) `quot` 32)) 
    MouseMotion x y _ _ -> handleMouseOver gs (fromIntegral x, fromIntegral y)
    _                               -> gs
    
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

