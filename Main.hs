module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Debug.Trace

-- import Data.List

import Model
import ConvertGrid -- (toGrid, findEnemy, handlePlayerAttack, killDeadEnemies)

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
  let pf' = map makeScarceBoard [(x,y) | x <- [0..width-1], y <- [0..height-1]] --, z <- [0,1], z == (x + y) `mod` 2] -- <- [0,1], z == (x+y) `mod` 2]

  let gs = Graphics char mainchar mob floor_ wall fnt
  let p = Player (1,1) 10 10 2 0 0 10
  let testEnemies = [Enemy (0,0) 10 10 5, Enemy (3,3) 15 15 5, Enemy (1,3) 20 20 5]

  gameLoop (GameState True [(10, 10)] fov' pf' testEnemies p (0, 0)) gs

drawSprite :: Surface -> Surface -> Coord -> IO(Bool)
drawSprite sprite dest (x, y) = blitSurface sprite Nothing dest (Just $ Rect (x*32) (y*32) 32 32 )

hpString :: Player -> String
hpString p = (show $ pHealth p) ++ " / " ++ (show $ pMaxHealth p)

enemyHpString :: Enemy -> String
enemyHpString e = (show $ eHealth e) ++ "/" ++ (show $ eMaxHealth e)

drawInfo :: Coord -> Surface -> Graphics -> [Enemy]-> IO(Bool)
drawInfo _ _ _ [] = return (True)
drawInfo pos s gx (e:es) = do
  if ePos e == pos then do
    strToBlit <- renderTextSolid (font gx) (enemyHpString e) (Color 255 0 0)
    blitSurface strToBlit Nothing s (Just $ Rect 500 400 0 0)
  else drawInfo pos s gx es

checkEnemiesLeft ::GameState -> GameState
checkEnemiesLeft gs
  | (length $ enemies gs) == 0 = gs {running = False }
  | otherwise = gs

dispString :: String -> Coord -> Font -> Surface -> IO Bool
dispString s c f surface= do
  title <- renderTextSolid f s (Color 255 0 0)
  blitSurface title Nothing surface (Just $ Rect (fst c) (snd c) 0 0)
  
drawPlayerInfo :: GameState -> Graphics -> Surface -> IO()
drawPlayerInfo gs gx s = do
  let p = gPlayer gs

  dispString (hpString p) (670, 500) (font gx) s
  dispString ((show $ pExp p) ++ " / " ++ (show $ (pLevelLimit p)) ) (670, 520) (font gx) s
  dispString ("Level  " ++ (show $ pLevel p)) (670, 540) (font gx) s

  return ()

gameLoop :: GameState -> Graphics-> IO ()
gameLoop gs gx = do

  s <- getVideoSurface
  gs' <- tickGame gs

  --reset screen
  fillRect s (Just $ Rect 0 0 800 608) (Pixel 0)
  
  let explored_pf = filter (\(x, y, _) -> (x,y,1) `elem` (fov gs)) (pf gs)

  let fillVal = (\(x, y, val) -> fillRect s (Just $ Rect (x*32) (y*32) 32 32) (Pixel (100*(1+(fromIntegral val)))))

  mapM_ fillVal explored_pf

  drawSprite (playerSurface gx) s (pPos $ gPlayer gs)

  let enemySprite = enemySurface gx
  mapM_ (\e -> drawSprite enemySprite s (ePos e)) ( filter (\e -> (fst $ ePos e, snd $ ePos e, 1) `elem` (fov gs)) (enemies gs))

  drawPlayerInfo gs gx s

  -- Draw info panel
  drawInfo (toGrid $ gMousePos gs)  s gx (enemies gs)

  SDL.flip s

  case running gs of
    True -> gameLoop gs' gx
    _ -> return()

levelPlayer :: Player -> Player
levelPlayer p = p { pLevelLimit = newLevelLimit, pExp = newExp, pLevel = newLevel }
  where newExp = (pExp p) - (pLevelLimit p)
        newLevelLimit = (pLevelLimit p) + 10
        newLevel = (pLevel p) + 1

handleLevelUp :: GameState -> GameState
handleLevelUp gs
  | pExp p >= pLevelLimit p = handleLevelUp gs { gPlayer = levelPlayer $ gPlayer gs }
  | otherwise = gs
    where p = gPlayer gs

tickGame :: GameState -> IO GameState
tickGame gs = do
  events <- getEvents pollEvent []
  let gs1 = foldl handleEvent gs events
  let gs2 = (handleLevelUp . checkEnemiesLeft . killDeadEnemies) gs1
  return gs2

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

