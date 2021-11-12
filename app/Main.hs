module Main where

import Pendulum2
import Pendulum2.View
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "double-pendulum" (640, 640) (0,0)

main :: IO ()
main = simulate window white step init getPic evol
    where
      step = 30
      init = ((pi/6.0, pi/3.0), (0, 0))
      ml = ((1.0, 1.0), (1.0, 1.0))
      getPic pq = flipRunPendulum2 ml pq getPicture
      evol _ dt pq = flipRunPendulum2 ml pq $ do
        symplecticEvol1 . realToFrac $ dt
        getPhase
