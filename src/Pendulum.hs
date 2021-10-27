module Pendulum (
  Pendulum,runPendulum,
  module PhysicalSystem
)where
import PhysicalSystem

type Pendulum = PhysicalSystem Double Double (Double,Double) Double Double
runPendulum :: Pendulum x -> (Double, Double) -> (Double, Double) -> (x, (Double, Double))
runPendulum system = runPhysicalSystem system (dqdt, dpdt)
  where
    dqdt (m, l) (q, p) = p / (m * l * l)
    dpdt (m, l) (q, p) = - m * 9.8 * l * sin q
