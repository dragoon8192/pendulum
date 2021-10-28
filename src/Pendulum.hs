module Pendulum (
  Pendulum,runPendulum,
  module PhysicalSystem
)where
import PhysicalSystem

type Pendulum = PhysicalSystem (Double,Double) Double Double
runPendulum :: (Double, Double) -> (Double, Double) -> Pendulum x -> x
runPendulum = runPhysicalSystem (dqdt, dpdt)
  where
    dqdt (m, l) (q, p) = p / (m * l * l)
    dpdt (m, l) (q, p) = - m * 9.8 * l * sin q
