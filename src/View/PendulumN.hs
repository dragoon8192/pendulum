module View.PendulumN(
PictureShowM(getPicture),
) where
import PendulumN
import Graphics.Gloss.Data.PointedPictures
import Data.Vector.Extra
import View.Pendulum

instance PictureShowM PendulumN where
  getPicture = do
    (vm, vl) <- askData
    vq <- getQ
    let vxy = toXY <$> vl <*> vq
    let xys = innerList vxy
    let vPic = pendulumToPicture <$> vm <*> vl <*> vq
    let pics = innerList vPic
    return . toPictures $ zip3 pics xys [0..]
