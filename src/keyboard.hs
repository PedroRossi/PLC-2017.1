module Keyboard (
  moveSpaceShipToRight,
  moveSpaceShipToLeft
) where

import Graphics.UI.Fungen

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
