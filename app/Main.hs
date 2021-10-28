module Main where

import Pendulum
import Pendulum.View
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "single-pendulum" (640, 640) (0,0)

main :: IO ()
main = do
  let step = 30
  simulate window white step getPicture (runPendulum (1.0, 1.0) (pi / 3.0, 0)) evol
      where
        evol :: ViewPort -> Float -> Pendulum Picture -> Pendulum Picture
        evol vp dt mPic= do
          mPic
          symplecticEvol1 . realToFrac $ dt
          getPicture
