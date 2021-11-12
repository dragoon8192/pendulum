module View.Pendulum2(
PictureShowM(getPicture),
) where
import Pendulum2
import Graphics.Gloss.Data.PointedPictures
import Data.Tuple.Extra
import View.Pendulum

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
