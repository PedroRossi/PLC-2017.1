module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Data.Foldable

data GameAttribute = Score Int
data GameState = Level Int | GameInit Int

type SpaceInvadersObject = GameObject ()
type SpaceInvadersAction a = IOGame GameAttribute () () () a

width = 480
height = 640
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble
spaceShipSize = 50.0

main :: IO ()
main = do
  let winConfig = ((100,0),(width,height),"A brief example!")
      bmpList = [("assets/background.bmp", Nothing)]
      gameMap = textureMap 0 1024.0 770.0 w h
      spaceShip = objectGroup "spaceShipGroup" [createSpaceShip]
      spaceShipFire = objectGroup "spaceShipFireGroup" []
      invader = objectGroup "invaderGroup" [createInvader (fromIntegral (i) :: Double) | i <- [1..10]]
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
  let bound = [(0,0),(spaceShipSize,0),(spaceShipSize/2,spaceShipSize)]
      pic = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "spaceShip" pic False (w/2, 20) (0,0) ()

createSpaceShipFire :: GLdouble -> GLdouble -> SpaceInvadersObject
createSpaceShipFire x y =
  let bound = [(0,0),(1,0),(1,1),(0,1)]
      pic = Basic (Polyg bound 1.0 1.0 1.0 Filled)
  in object "spaceShipFire" pic False (x+(spaceShipSize/2),y) (0,10) ()

createInvader :: GLdouble -> SpaceInvadersObject
createInvader n =
  let bound = [(0,0),(-12.5,0),(-7.25,-12.5)]
      pic = Basic (Polyg bound 255.0 1.0 1.0 Filled)
  in object "invader" pic False ((n*spaceShipSize),h) (6,-0.2) ()

createInvaderFire :: GLdouble -> GLdouble -> SpaceInvadersObject
createInvaderFire x y = do
  let pic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  object "invaderFire" pic False (x,y) (0,-10) ()

-- makeItRain :: SpaceInvadersAction ()
-- makeItRain = do
--   invaders <- getObjectsFromGroup "invaderGroup"
--   arr <- [(i, (randomInt(0,1)) | i <- invaders]
--   -- (randomInt (0, length(invaders)))
--   forM_ arr $ \pos -> do
--     -- let i = fst pos
--     let invader = invaders!!0
--     (pX,pY) <- getObjectPosition invader
--     let obj = (createInvaderFire (pX) (pY))
--     addObjectsToGroup [obj] "invaderFireGroup"
--     drawObject obj

moveSpaceShipToRight :: Modifiers -> Position -> SpaceInvadersAction ()
moveSpaceShipToRight _ _ = do
  obj     <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveSpaceShipToLeft :: Modifiers -> Position -> SpaceInvadersAction ()
moveSpaceShipToLeft _ _ = do
  obj <- findObject "spaceShip" "spaceShipGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
   then (setObjectPosition ((pX - 5),pY) obj)
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
  funExit
  setGameAttribute (Score 0)

gameCycle :: SpaceInvadersAction ()
gameCycle = do
  (Score n) <- getGameAttribute
  printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0

  invaders <- getObjectsFromGroup "invaderGroup"
  spaceShipBullets <- getObjectsFromGroup "spaceShipFireGroup"
  invaderBullets <- getObjectsFromGroup "invaderFireGroup"
  spaceShip <- findObject "spaceShip" "spaceShipGroup"

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
        -- atingiu alguem
        setObjectAsleep True invader
        setGameAttribute (Score (n+10))

  showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
