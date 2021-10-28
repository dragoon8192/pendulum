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
  return ()
  -- flipRunPendulumT (1.0, 1.0) (pi / 3.0, 0) $ do
    -- lift $ simulate window white step init (const getPicture) evol
        --evol :: ViewPort -> Float -> Pendulum Picture -> Pendulum Picture
       --   symplecticEvol1 . realToFrac $ dt
