module Graphics.Gloss.Data.PointedPictures (
  PointedPictures,
  toPictures
) where
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

newtype PointedPictures = PointedPictures {innerList :: [(Picture, Point, Float)]}
  deriving (Show, Eq)
toPictures :: PointedPictures -> Picture
toPictures = Pictures . foldr combine [] . innerList
  where
    combine :: (Picture, Point, Float) -> [Picture] -> [Picture]
    combine (pic, (x, y), theta) ls = (pic :) $ translate x y . rotate theta <$> ls


