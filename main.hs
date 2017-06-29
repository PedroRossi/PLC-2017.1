module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Data.Foldable

data GameAttribute = Score Int
data GateState = Level Int | GameInit Int

type SpaceInvadersObject = GameObject ()
type SpaceInvadersAction a = IOGame GameAttribute () () () a

width = 480
height = 640
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

main :: IO ()
main = do
  let winConfig = ((100,0),(width,height),"A brief example!")
      bmpList = [("assets/background.bmp", Nothing)]
      gameMap = textureMap 0 1024.0 770.0 w h
      spaceShip = objectGroup "spaceShipGroup" [createSpaceShip]
      spaceShipFire = objectGroup "spaceShipFireGroup" []
      invader = objectGroup "invaderGroup" [createInvader (fromIntegral (i) :: Double) | i <- [0..10]]
      invaderFire = objectGroup "invaderFireGroup" []
      initScore = Score 0
      input = [
        (SpecialKey KeyRight, StillDown, moveSpaceShipToRight),
        (SpecialKey KeyLeft, StillDown, moveSpaceShipToLeft),
        (SpecialKey KeyUp, Press, fireSpaceShip),
        (Char ' ', Press, fireSpaceShip),
        (Char 'q', Press, \_ _ -> funExit)
        ]
  funInit winConfig gameMap [spaceShip, invader, spaceShipFire, invaderFire] () initScore input gameCycle (Timer 40) bmpList

createSpaceShip :: SpaceInvadersObject
createSpaceShip =
  let bound = [(0,0),(50,0),(25,50)]
      pic = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "spaceShip" pic False (w/2, 20) (0,0) ()

createSpaceShipFire :: GLdouble -> GLdouble -> SpaceInvadersObject
createSpaceShipFire x y =
  let bound = [(0,0),(1,0),(1,1),(0,1)]
      pic = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "spaceShipFire" pic False (x+25.0,y) (0,10) ()

createInvader :: Double -> SpaceInvadersObject
createInvader n =
  let bound = [(0,0),(-25,0),(-12.5,-25)]
      pic = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "invader" pic False ((n*30.0),h) (6,-0.2) ()

createInvaderFire :: SpaceInvadersObject
createInvaderFire = do
  let invaderPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  object "invader" invaderPic False (0,h) (8,-0.2) ()


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

fireSpaceShip :: Modifiers -> Position -> IOGame GameAttribute () () () ()
fireSpaceShip _ _ = do
  obj <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  let obj = (createSpaceShipFire (pX) (pY))
  addObjectsToGroup [obj] "spaceShipFireGroup"
  drawObject obj

gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
  (Score n) <- getGameAttribute
  printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0

  invaders <- getObjectsFromGroup "invaderGroup"
  spaceShipBullets <- getObjectsFromGroup "spaceShipFireGroup"
  invaderBullets <- getObjectsFromGroup "invaderFireGroup"

  forM_ invaders $ \invader -> do
    col1 <- objectLeftMapCollision invader
    col2 <- objectRightMapCollision invader
    when (col1 || col2) (reverseXSpeed invader)
    col4 <- objectBottomMapCollision invader
    when col4 $ do
      funExit
      setGameAttribute (Score 0)



  showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
