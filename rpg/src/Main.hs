import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Dungeon
import Render as R

import Debug.Trace

main :: IO()
main = do

  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  enableKeyRepeat 500 30

  gx <- loadGx
  gameLoop $ makeNewGame gx

loadGx :: IO [Surface]
loadGx = do
  tiles <- SDLi.load "image/terrain.png"
  player <- SDLi.load "image/yuffie.png"
  return [tiles, player]

gameLoop :: GameState -> IO ()
gameLoop gs = do
  gs' <- tickGame gs
  R.renderWorld gs

  case gRunning gs' of
    True -> gameLoop gs'
    _ -> return()

tickGame :: GameState -> IO GameState
tickGame gs = do
  events <- getEvents pollEvent []
  let gs' = foldl handleEvent gs events
  return $ updateState gs'

-- Look for conditional events within the state
updateState :: GameState -> GameState
updateState gs 
  | screenTransition gs = transition gs
  | otherwise = gs

handleEvent :: GameState -> Event -> GameState
handleEvent gs e =
  case e of
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs {gRunning = False}
    KeyDown (Keysym SDLK_t _ _) -> move South gs
    KeyDown (Keysym SDLK_n _ _) -> move North gs
    KeyDown (Keysym SDLK_h _ _) -> move West gs
    KeyDown (Keysym SDLK_s _ _) -> move East gs
    KeyDown (Keysym SDLK_a _ _) -> talk gs
    _                               -> gs

talk :: GameState -> GameState
talk = undefined
    
move :: Direction -> GameState -> GameState
move South = modPlayerPos South (0, 1)
move North = modPlayerPos North (0, -1)
move East = modPlayerPos East (1, 0)
move West = modPlayerPos West (-1, 0)

-- Test if spot is free. if so walk
modPlayerPos :: Direction -> Coord -> GameState -> GameState
modPlayerPos newFace (xmod, ymod) gs = case isFree newPos gs of
  True -> gs { gPlayer = player {pPos = newPos, pFacing = newFace}}
  False -> gs { gPlayer = player {pFacing = newFace}}
  where player = gPlayer gs
        pos = pPos player
        newPos = ((fst pos) + xmod, (snd pos) + ymod)

-- is the tile free for walking? 
isFree :: Coord -> GameState -> Bool
isFree (x, y) gs = t /= Wall && noNpc (x, y) gs
  where (_, t) = head $ filter (\((x', y'), _) -> x == x' && y == y') (aBoard $ gArea gs)

noNpc :: Coord -> GameState -> Bool
noNpc testPos gs = (length npc) == 0
  where npc = filter ((==) testPos) positions
        positions = map (\x -> nPos x) (aNpcs $ gArea gs)

getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)
