module View.Pendulum (
  scaleL, scaleM, toXY, pendulumToPicture,
  PictureShowM(getPicture),
) where
import Pendulum
import Graphics.Gloss
import Graphics.Gloss.Data.PointedPictures

scaleL = realToFrac . (100.0 *)
scaleM = realToFrac . (10 *) . sqrt

toXY l q = (scaleL (l * sin q), - scaleL (l * cos q))

pendulumToPicture m l q = pictures [
  line [(0,0), (x,y)],
  translate x y . circleSolid . scaleM $ m
  ]
    where
      (x, y) = toXY l q

instance PictureShowM Pendulum where
  getPicture = do
    (m, l) <- askData
    q <- getQ
    return $ pendulumToPicture m l q
