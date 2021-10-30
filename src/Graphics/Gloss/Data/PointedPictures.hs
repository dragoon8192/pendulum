module Graphics.Gloss.Data.PointedPictures (
  PointedPictures,
  toPictures,
  PictureShow,
  pictureShow,
  PictureShowM,
  getPicture
) where
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

type PointedPictures = [(Picture, Point, Float)]
toPictures :: PointedPictures -> Picture
toPictures = pictures . foldr combine []
  where
    combine :: (Picture, Point, Float) -> [Picture] -> [Picture]
    combine (pic, (x, y), theta) ls = (pic :) $ translate x y . rotate theta <$> ls

class PictureShow a where
  pictureShow :: a -> Picture

class PictureShowM m where
  getPicture :: m Picture
