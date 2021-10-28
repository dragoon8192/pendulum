module Pendulum.View (
  getPicture
) where
import Pendulum
import Graphics.Gloss
import Graphics.Gloss.Data.PointedPictures

scaleL = (100.0 *)
scaleM = (10 *) . sqrt

instance PictureShowM Pendulum where
  getPicture = do
    (m, l) <- askData
    let pixelM = scaleM m
    let pixelL = scaleL l
    q <- getQ
    let (x,y) = (realToFrac (pixelL * sin q), realToFrac (- pixelL * cos q))
    let theta = realToFrac $ q * 180.0 / pi
    let pic = pictures [
              line [(0, 0), (x, y)],
              translate x y . circleSolid . realToFrac $ pixelM]
    return pic
