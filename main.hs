module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Data.Foldable

data GameAttribute = Score Int
data GameState = Level Int

type SpaceInvadersObject = GameObject ()
type SpaceInvadersAction a = IOGame GameAttribute () () () a

width = 480
height = 640
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble
spaceShipSize = 47.0
invaderXSpeed = 8.0
bmpList = [("assets/background.bmp", Nothing),
           ("assets/player2.bmp", Just[(255,0,255)]),
           ("assets/invader.bmp", Just[(255,0,255)])]

main :: IO ()
main = do
  let winConfig = ((100,0),(width,height),"A brief example!")
      gameMap = textureMap 0 1024.0 770.0 w h
      spaceShip = objectGroup "spaceShipGroup" [createSpaceShip]
      spaceShipFire = objectGroup "spaceShipFireGroup" []
      invader = objectGroup "invaderGroup" [createInvader (fromIntegral (i) :: Double) | i <- [1..5]]
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
  let pic = Tex (47,28) 1
  in object "spaceShip" pic False (w/2, 20) (0,0) ()

createSpaceShipFire :: GLdouble -> GLdouble -> SpaceInvadersObject
createSpaceShipFire x y =
  let bound = [(0,0),(3,0),(3,3),(0,3)]
      pic = Basic (Polyg bound 10.0 10.0 10.0 Filled)
  in object "spaceShipFire" pic False (x,y) (0,30) ()

createInvader :: GLdouble -> SpaceInvadersObject
createInvader x = do
  let pic = Tex (50,37) 2
  object "invader" pic False ((x*64),h) (invaderXSpeed,(-0.2)) ()

createInvaderFire :: GLdouble -> GLdouble -> SpaceInvadersObject
createInvaderFire x y = do
  let pic = Basic (Circle 1.0 255.0 0.0 0.0 Filled)
  object "invaderFire" pic False (x,y) (0,-10) ()

binaryBool :: Double -> Bool
binaryBool n
  | n>=99.0 = True
  | otherwise = False

makeItRain :: SpaceInvadersAction ()
makeItRain = do
  invaders <- getObjectsFromGroup "invaderGroup"
  forM_ invaders $ \invader -> do
    sleeping <- getObjectAsleep (invader)
    when (not sleeping) $ do
      (Score n) <- getGameAttribute
      let aux = ((fromIntegral n :: Double)/100.0)
      shoot <- randomDouble (0.0,100.0 + (aux))
      when (binaryBool (shoot)) $ do
         (pX,pY) <- getObjectPosition invader
         let obj = (createInvaderFire (pX) (pY))
         addObjectsToGroup [obj] "invaderFireGroup"
         drawObject obj

moveSpaceShipToRight :: Modifiers -> Position -> SpaceInvadersAction ()
moveSpaceShipToRight _ _ = do
  obj     <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 7 <= w)
   then (setObjectPosition ((pX + 7),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveSpaceShipToLeft :: Modifiers -> Position -> SpaceInvadersAction ()
moveSpaceShipToLeft _ _ = do
  obj <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 7 >= 0)
   then (setObjectPosition ((pX - 7),pY) obj)
   else (setObjectPosition (sX/2,pY) obj)

fireSpaceShip :: Modifiers -> Position -> SpaceInvadersAction ()
fireSpaceShip _ _ = do
  obj <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  let obj = (createSpaceShipFire (pX) (pY))
  addObjectsToGroup [obj] "spaceShipFireGroup"
  drawObject obj

endGame :: SpaceInvadersAction ()
endGame = do
  -- invaders <- getObjectsFromGroup "invaderGroup"
  -- invadersFire <- getObjectsFromGroup "invaderFireGroup"
  -- spaceShipFire <- getObjectsFromGroup "spaceShipFireGroup"
  -- destroyObjects invaders
  -- destroyObjects invadersFire
  -- destroyObjects spaceShipFire
  -- setGameAttribute (Score 0)
  -- addObjectsToGroup [createInvader (fromIntegral (i) :: Double) | i <- [1..5]] "invaderGroup"

gameCycle :: SpaceInvadersAction ()
gameCycle = do
  (Score n) <- getGameAttribute
  printOnScreen (show("Score ")++show n) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show("Level ") ++ show((fromIntegral n :: Double)/100)) TimesRoman24 (0,h-20.0) 0.0 1.0 0.0

  when ((n`mod`100)==0 && n>0) $ do
    invaders <- getObjectsFromGroup "invaderGroup"
    destroyObjects invaders
    addObjectsToGroup [createInvader (fromIntegral (i) :: Double) | i <- [1..5]] "invaderGroup"
    objs <- getObjectsFromGroup "invaderGroup"
    forM_ objs $ \invader -> do
      let aux = (fromIntegral n :: Double)/100.0
      setObjectSpeed (8.0,-0.2-(aux/2.0)) invader
    drawAllObjects

  invaders <- getObjectsFromGroup "invaderGroup"
  spaceShipBullets <- getObjectsFromGroup "spaceShipFireGroup"
  invaderBullets <- getObjectsFromGroup "invaderFireGroup"
  spaceShip <- findObject "spaceShip" "spaceShipGroup"

  makeItRain
  forM_ invaders $ \invader -> do
    wallHit1 <- objectLeftMapCollision invader
    wallHit2 <- objectRightMapCollision invader
    when (wallHit1 || wallHit2) (reverseXSpeed invader)
    invassionSuccess <- objectBottomMapCollision invader
    crash <- objectsCollision invader spaceShip
    when (invassionSuccess || crash) $ endGame
    forM_ invaderBullets $ \b -> do
      spaceShipHit <- objectsCollision spaceShip b
      when spaceShipHit $ endGame
    forM_ spaceShipBullets $ \b -> do
      invaderHit <- objectsCollision invader b
      when invaderHit $ do
        setObjectAsleep True invader
        setGameAttribute (Score (n+20))
  forM_ invaders $ \invader1 -> do
    forM_ invaders $ \invader2 -> do
      invadersCrash <- objectsCollision invader1 invader2
      when invadersCrash $ do
        (reverseXSpeed invader1)
        (reverseXSpeed invader2)

  showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
