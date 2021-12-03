module Graphics.Gloss.Data.PointedPictures (
  PointedPictures,
  toPictures,
) where
import Graphics.Gloss
    ( rotate, translate, pictures, Point, Picture )

type PointedPictures = [(Picture, Point, Float)]
toPictures :: PointedPictures -> Picture
toPictures = pictures . foldr combine []
  where
    combine :: (Picture, Point, Float) -> [Picture] -> [Picture]
    combine (pic, (x, y), theta) ls = (pic :) $ translate x y . rotate theta <$> ls
