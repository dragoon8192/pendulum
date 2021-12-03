module View.PendulumN(
  getPictureWithEnergy,
  PictureShowM(getPicture),
) where
import PendulumN
import Graphics.Gloss
import Graphics.Gloss.Data.PointedPictures
import Data.Vector.Extra
import View.Pendulum

instance PictureShowM PendulumN where
  getPicture = do
    (vm, vl) <- askData
    vq <- getQ
    let vxy = toXY <$> vl <*> vq
    let xys = toList vxy
    let vPic = pendulumToPicture <$> vm <*> vl <*> vq
    let pics = toList vPic
    return . toPictures $ zip3 pics xys [0..]

getPictureWithEnergy = do
  pic <- getPicture
  e <- hamiltonian
  let picText = scale 0.25 0.25 . text . show $ e
  return $ pictures [pic, picText]
