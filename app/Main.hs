module Main where

import Pendulum
import Pendulum.View
import Graphics.Gloss

window :: Display
window = InWindow "single-pendulum" (640, 640) (0,0)

main :: IO ()
main = do
  display window white $ circleSolid 100
