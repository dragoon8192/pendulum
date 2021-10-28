module Main where

import Pendulum
import Pendulum.View
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "single-pendulum" (640, 640) (0,0)


main :: IO ()
main = simulate window white step init getPic evol
    where
      step = 30
      init = (pi/6.0, 0)
      ml = (1.0, 1.0)
      getPic pq = flipRunPendulum ml pq getPicture
      evol _ dt pq = flipRunPendulum ml pq $ do
        symplecticEvol1 . realToFrac $ dt
        getPhase
