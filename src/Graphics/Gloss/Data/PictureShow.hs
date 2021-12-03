module Graphics.Gloss.Data.PictureShow(
  PictureShow,
  pictureShow,
  PictureShowM,
  getPicture
) where
import Graphics.Gloss ( Picture )

class PictureShow a where
  pictureShow :: a -> Picture

class PictureShowM m where
  getPicture :: m Picture
