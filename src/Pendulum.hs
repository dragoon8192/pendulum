module Pendulum (
  Pendulum, runPendulum, flipRunPendulum,
  PendulumT, runPendulumT, flipRunPendulumT,
  Pendulum2, runPendulum2, flipRunPendulum2,
  Pendulum2T, runPendulum2T, flipRunPendulum2T,
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

type Pendulum2T m = PhysicalSystemT ((Double, Double), (Double, Double)) (Double, Double) (Double, Double) m
type Pendulum2 = Pendulum2T Identity

runPendulum2T :: (Monad m) => Pendulum2T m x -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> m x
runPendulum2T = flip runPhysicalSystemT (dqdt, dpdt)
  where
    dqdt ((m1, m2), (l1, l2)) (_, (p1, p2)) = (p1 / ((m1 + m2) * l1 * l1), p2 / (m2 * l2 * l2))
    dpdt ((m1, m2), (l1, l2)) ((q1, q2), _) = (- (m1 + m2) * 9.8 * l1 * sin q1, - m2 * 9.8 * l2 * sin q2)
flipRunPendulum2T :: (Monad m) => ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Pendulum2T m x -> m x
flipRunPendulum2T = flip . flip runPendulum2T

runPendulum2 :: Pendulum2T Identity a -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> a
runPendulum2 pmx (m, l) (q, p) = runIdentity $ runPendulum2T pmx (m, l) (q, p)
flipRunPendulum2 :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Pendulum2T Identity a -> a
flipRunPendulum2 = flip . flip runPendulum2
