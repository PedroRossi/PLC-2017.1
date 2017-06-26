module Main where

import Graphics.UI.Fungen

main :: IO ()
main =
  let winConfig = ((50,50),(500,300),"Hello, Fungen World! Press Q to quit")
      gameMap = colorMap 0.0 0.0 0.0 250 250
      bindings = [(Char 'q', Press, \_ _ -> funExit)]
  in funInit winConfig gameMap [] () () bindings (return()) Idle []
