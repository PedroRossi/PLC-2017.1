module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
-- import System.Random

data GameAttribute = Score Int

width = 480
height = 640
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble
invaderInitialPos :: Double
invaderInitialPos = 0.0

main :: IO ()
main = do
  let winConfig = ((100,0),(width,height),"A brief example!")
      bmpList = [("assets/background.bmp", Nothing)]
      gameMap = textureMap 0 1024.0 770.0 w h
      spaceShip = objectGroup "spaceShipGroup" [createSpaceShip]
      spaceShipFire = objectGroup "spaceShipFireGroup" [createSpaceShipFire]
      invader = objectGroup "invaderGroup" [createInvader]
      invaderFire = objectGroup "invaderFireGroup" [createInvaderFire]
      initScore = Score 0
      input = [
        (SpecialKey KeyRight, StillDown, moveSpaceShipToRight),
        (SpecialKey KeyLeft, StillDown, moveSpaceShipToLeft),
        -- (SpecialKey SpaceBar, StillDown, shoot),
        (Char 'q', Press, \_ _ -> funExit)
        ]
  funInit winConfig
          gameMap
          [spaceShip, invader, spaceShipFire, invaderFire]
          ()
          initScore
          input
          gameCycle
          (Timer 40)
          bmpList


createSpaceShip :: GameObject ()
createSpaceShip =
  let bound = [(0,0),(100,0),(50,100)]
      pic   = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "spaceShip" pic False (w/2, 20) (0,0) ()

createSpaceShipFire :: GameObject ()
createSpaceShipFire =
  let bound = [(0,0),(1,0),(1,1),(0,1)]
      pic   = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "spaceShip" pic False (w/2, 20) (0,0) ()

createInvader :: GameObject ()
createInvader = do
  let invaderPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  let aux = invaderInitialPos
  let invaderInitialPos = aux+1.0
  object "invader" invaderPic False (aux,h) (8,-0.2) ()

createInvaderFire :: GameObject ()
createInvaderFire = do
  let invaderPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  let aux = invaderInitialPos
  let invaderInitialPos = aux+1.0
  object "invader" invaderPic False (aux,h) (8,-0.2) ()

moveSpaceShipToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveSpaceShipToRight _ _ = do
  obj     <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveSpaceShipToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveSpaceShipToLeft _ _ = do
  obj <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
   then (setObjectPosition ((pX - 5),pY) obj)
   else (setObjectPosition (sX/2,pY) obj)

gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
  (Score n) <- getGameAttribute
  -- printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0

  printOnScreen (show (invaderInitialPos)) TimesRoman24 (0,0) 1.0 1.0 1.0

  invader <- findObject "invader" "invaderGroup"
  col1 <- objectLeftMapCollision invader
  col2 <- objectRightMapCollision invader
  when (col1 || col2) (reverseXSpeed invader)
  col3 <- objectTopMapCollision invader
  -- when col3 (reverseYSpeed invader)
  col4 <- objectBottomMapCollision invader
  when col4 (reverseYSpeed invader)

  showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
