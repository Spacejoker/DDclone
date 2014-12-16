import Dungeon

main :: IO()
main = do
  gameLoop makeNewGame

gameLoop :: GameState -> IO ()
gameLoop gs = do

  x <- getLine
  putStrLn $ show gs

  case gRunning gs of
    True -> gameLoop gs
    _ -> return()

