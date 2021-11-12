module Pendulum (
  Pendulum, runPendulum, flipRunPendulum,
  PendulumT, runPendulumT, flipRunPendulumT,
  module PhysicalSystem
)where
import PhysicalSystem
import Data.Functor.Identity

type PendulumT m = PhysicalSystemT (Double,Double) Double Double m
type Pendulum = PendulumT Identity

runPendulumT :: (Monad m) => PendulumT m x -> (Double, Double) -> (Double, Double) -> m x
runPendulumT = flip runPhysicalSystemT (dqdt, dpdt)
  where
    dqdt (m, l) (q, p) = p / (m * l * l)
    dpdt (m, l) (q, p) = - m * 9.8 * l * sin q
flipRunPendulumT :: (Monad m) => (Double, Double) -> (Double, Double) -> PendulumT m x -> m x
flipRunPendulumT = flip . flip runPendulumT

runPendulum :: Pendulum x -> (Double, Double) -> (Double, Double) -> x
runPendulum pmx (m, l) (q, p) = runIdentity $ runPendulumT pmx (m, l) (q, p)
flipRunPendulum :: (Double, Double) -> (Double, Double) -> Pendulum c -> c
flipRunPendulum = flip . flip runPendulum
