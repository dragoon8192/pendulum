module Pendulum (
  Pendulum,runPendulum,
  PendulumT, runPendulumT,
  module PhysicalSystem
)where
import PhysicalSystem
import Data.Functor.Identity

type PendulumT m = PhysicalSystemT (Double,Double) Double Double m
type Pendulum = PendulumT Identity
runPendulumT :: (Monad m) => (Double, Double) -> (Double, Double) -> PendulumT m x -> m x
runPendulumT = runPhysicalSystemT (dqdt, dpdt)
  where
    dqdt (m, l) (q, p) = p / (m * l * l)
    dpdt (m, l) (q, p) = - m * 9.8 * l * sin q
runPendulum :: (Double, Double) -> (Double, Double) -> Pendulum x -> x
runPendulum (m, l) (q, p) x = runIdentity $ runPendulumT (m, l) (q, p) x
