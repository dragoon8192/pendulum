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
  simulate window white step init getPic evol
    where
      init = flipRunPendulum (1.0, 1.0) (pi/6.0, 0)
      getPic = ($! getPicture)
      evol _ dt f s = f $ (symplecticEvol1 . realToFrac $ dt) >> s
