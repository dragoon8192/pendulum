module Pendulum.View (
PictureShowM(getPicture),
) where
import Pendulum
import Graphics.Gloss
import Graphics.Gloss.Data.PointedPictures
import Data.Tuple.Extra

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
    let (x,y) = (realToFrac (l * sin q), realToFrac (- l * cos q))
    --let theta = realToFrac $ q * 180.0 / pi
    return $ pictures [
              line [(0, 0), (x, y)],
              translate x y . circleSolid . scaleM $ m
              ]

instance PictureShowM Pendulum2 where
  getPicture = do
    (ms, ls) <- askData
    qs <- getQ
    let xys@(xy1, xy2) = toXY <@$> ls <@*> qs
    let (pic1,pic2) = pendulumToPicture <@$> ms <@*> ls <@*> qs
    return $ toPictures [
      (pic1, xy1, 0),
      (pic2, xy2, 0)
      ]
