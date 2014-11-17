module Render where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Model
import ViewFormatter

drawInfo :: Coord -> Surface -> Graphics -> [Enemy]-> IO(Bool)
drawInfo _ _ _ [] = return (True)
drawInfo pos s gx (e:es) = do
  if ePos e == pos then do
    strToBlit <- renderTextSolid (font gx) (enemyHpString e) (Color 255 0 0)
    blitSurface strToBlit Nothing s (Just $ Rect 500 400 0 0)
  else drawInfo pos s gx es

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

drawSprite :: Surface -> Surface -> Coord -> IO(Bool)
drawSprite sprite dest (x, y) = blitSurface sprite Nothing dest (Just $ Rect (x*32) (y*32) 32 32 )

