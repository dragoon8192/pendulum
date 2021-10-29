module Pendulum.View (
PictureShowM(getPicture),
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
    return $ pictures [
              line [(0, 0), (x, y)],
              translate x y . circleSolid . realToFrac $ pixelM
              ]

instance PictureShowM Pendulum2 where
  getPicture = do
    ((m1, m2), (l1, l2)) <- askData
    let pixelM1 = scaleM m1
    let pixelM2 = scaleM m2
    let pixelL1 = scaleL l1
    let pixelL2 = scaleL l2
    (q1, q2) <- getQ
    let (x1,y1) = (realToFrac (pixelL1 * sin q1), realToFrac (- pixelL1 * cos q1))
    let (x2,y2) = (realToFrac (pixelL2 * sin q2), realToFrac (- pixelL2 * cos q2))
    let theta1 = realToFrac $ q1 * 180.0 / pi
    let theta2 = realToFrac $ q2 * 180.0 / pi
    return $ pictures [
              line [(0, 0), (x1, y1), (x1 + x2, y1 + y2)],
              translate x1 y1 . circleSolid . realToFrac $ pixelM1,
              translate (x1 + x2) (y1 + y2) . circleSolid . realToFrac $ pixelM2
              ]
