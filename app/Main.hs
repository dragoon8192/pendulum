module Main where

import PendulumN
import View.PendulumN
import Data.Vector.Extra
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "double-pendulum" (640, 640) (0,0)

main :: IO ()
main = simulate window white step init getPic evol
    where
      step = 30
      vq = vector [pi/6.0, pi/3.0]
      vp = vector [0..]
      init = (vq, vp)
      vm = vector [1.0, 1.0]
      vl = vector [1.0, 1.0]
      ml = (vm, vl)
      getPic pq = flipRunPendulumN ml pq getPicture
      evol _ dt pq = flipRunPendulumN ml pq $ do
        symplecticEvol1 . realToFrac $ dt
        getPhase
