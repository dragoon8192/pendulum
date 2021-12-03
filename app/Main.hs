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
      n = 3
      vq = vector . take n $ cycle [pi/3.0, pi/4.0, pi/5.0]
      vp = vector . replicate n $ 0
      init = (vq, vp)
      vm = vector . replicate n $ 1.0
      vl = vector . replicate n $ 1.0
      ml = (vm, vl)
      getPic pq = flipRunPendulumN ml pq getPictureWithEnergy
      evol _ dt pq = flipRunPendulumN ml pq $ do
        symplecticEvol1 . realToFrac $ dt
        getPhase
