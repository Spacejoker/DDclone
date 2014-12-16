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
  return [tiles]

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
  return $ foldl handleEvent gs events

handleEvent :: GameState -> Event -> GameState
handleEvent gs e =
  case e of
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs {gRunning = False}
    KeyDown (Keysym SDLK_t _ _) -> move South gs
    KeyDown (Keysym SDLK_n _ _) -> move North gs
    KeyDown (Keysym SDLK_h _ _) -> move West gs
    KeyDown (Keysym SDLK_s _ _) -> move East gs
    _                               -> gs
    
move :: Direction -> GameState -> GameState
move South gs = modPlayerPos (0, 1) gs
move North gs = modPlayerPos (0, -1) gs
move East gs = modPlayerPos (1, 0) gs
move West gs = modPlayerPos (-1, 0) gs

modPlayerPos :: Coord -> GameState -> GameState
modPlayerPos (xmod, ymod) gs = case isFree newPos gs of
  True -> gs{ gPlayer = player {pPos = newPos}}
  False -> gs
  where player = gPlayer gs
        pos = pPos player
        newPos = ((fst pos) + xmod, (snd pos) + ymod)

isFree :: Coord -> GameState -> Bool
isFree (x, y) gs = t == Floor
  where area = gArea gs
        (_, t) = head $ filter (\((x', y'), _) -> x == x' && y == y') ( gBoard area)

getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)
