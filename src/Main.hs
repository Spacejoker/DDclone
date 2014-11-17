module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Debug.Trace

import Model
import ConvertGrid
import Render
import GameLogic

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
  let makeScarceBoard = \(x, y) -> (x, y, decideZ x y) 
       where decideZ x' y' = if (abs ((5 -x') + (5 - y'))) `mod` 4 == 0 then 1 else 0
  let pf' = map makeScarceBoard [(x,y) | x <- [0..width-1], y <- [0..height-1]] 

  let gs = Graphics char mainchar mob floor_ wall fnt
  let p = Player (1,1) 10 10 2 0 0 10
  let testEnemies = [Enemy (0,0) 10 10 5, Enemy (3,2) 15 15 5, Enemy (1,3) 20 20 5]

  gameLoop (GameState True [(10, 10)] fov' pf' testEnemies p (0, 0) False) gs

gameLoop :: GameState -> Graphics-> IO ()
gameLoop gs gx = do

  s <- getVideoSurface
  gs' <- tickGame gs

  --reset screen
  fillRect s (Just $ Rect 0 0 800 608) (Pixel 0)
  
  let explored_pf = filter (\(x, y, _) -> (x,y,1) `elem` (fov gs)) (pf gs)

  let fillVal = (\(x, y, val) -> fillRect s (Just $ Rect (x*32) (y*32) 32 32) 
                                    (Pixel (100*(1+(fromIntegral val)))))

  mapM_ fillVal explored_pf

  drawSprite (playerSurface gx) s (pPos $ gPlayer gs)

  let enemySprite = enemySurface gx

  mapM_ 
    (\e -> drawSprite enemySprite s (ePos e)) 
    ( filter (\e -> (fst $ ePos e, snd $ ePos e, 1) `elem` (fov gs)) (enemies gs))

  drawPlayerInfo gs gx s

  -- Draw info panel
  drawInfo (toGrid $ gMousePos gs)  s gx (enemies gs)

  SDL.flip s

  case running gs of
    True -> gameLoop gs' gx
    _ -> return()

tickGame :: GameState -> IO GameState
tickGame gs = do
  events <- getEvents pollEvent []
  let gs1 = foldl handleEvent gs events
  let gs2 = (handleLevelUp . checkEnemiesLeft . killDeadEnemies) gs1
  let running' = (running gs2) && (pHealth $ gPlayer gs2) > 0
  return gs2 {running = running'}

valueOf ::  (Int, Int) -> [(Int, Int, a)] -> (Int, Int, a)
valueOf (mx, my) list = head $ filter (\(x',y',_) -> x' == mx && y' == my) list

countFovCount :: [(Int, Int, Int)] -> Int
countFovCount fov' = length $ filter (\(_,_,x) -> x == 0) fov'

exploreUpdatePlayer :: Player -> Int -> Player
exploreUpdatePlayer p n = p { pHealth = newHealth }
  where newHealth = trace (show n) min ((pHealth p) + n ) (pMaxHealth p)

findNewFov :: (Int, Int) -> GameState -> Int -> [(Int, Int, Int)]
findNewFov (mx, my) gs clickTile = map (u (mx, my)) (fov gs)
  where (_,_,zz) = valueOf (mx, my) (fov gs) 
        u = (\(x, y) (x', y', val) -> 
          if zz == 1 && 
              abs (x' - x) <= 1 && 
              abs (y' - y) <= 1 &&
              clickTile == 0
            then (x', y', 1) 
            else (x', y', val))

--Player makes a move
handleClick :: GameState -> Coord -> GameState
handleClick gs (mx, my)
  -- do nothing if it is fov or wall
  | notFree  || hasWall = gs
  | hasEnemy = (handlePlayerAttack (mx, my) gs{ gPlayer = player' })
  | otherwise = gs {fov = fov', gPlayer = movePlayer player' (mx, my)}
  where notFree = (mx, my, 0) `elem` (fov gs)
        hasWall = pfv /= 0
        (_,_,pfv) = valueOf (mx, my) (pf gs)
        hasEnemy = length (filter (\e -> (mx, my) == ePos e) (enemies gs)) > 0
        fov' = findNewFov (mx, my) gs pfv
        exploredCnt = if hasEnemy then 0 else (countFovCount $ fov gs) - (countFovCount fov')
        player' = exploreUpdatePlayer (gPlayer gs) exploredCnt -- regen player for exploration

handleMouseOver :: GameState -> (Int, Int) -> GameState
handleMouseOver gs pos = gs { gMousePos = pos }

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

