module Pendulum2 (
  Pendulum2, runPendulum2, flipRunPendulum2,
  Pendulum2T, runPendulum2T, flipRunPendulum2T,
  module PhysicalSystem
)where
import PhysicalSystem
import Data.Functor.Identity
import Data.Tuple.Extra

type Pendulum2T m = PhysicalSystemT ((Double, Double), (Double, Double)) (Double, Double) (Double, Double) m
type Pendulum2 = Pendulum2T Identity

runPendulum2T :: (Monad m) => Pendulum2T m x -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> m x
runPendulum2T = flip runPhysicalSystemT (dqdt, dpdt)
  where
    dqdt (ms, ls) (_, ps) = f <@$> mass' ms <@*> ls <@*> ps
    dpdt (ms, ls) (qs, _) = g <@$> mass' ms <@*> ls <@*> qs
    mass' (m1, m2) = (m1 + m2, m2)
    f m l p = p / (m * l * l)
    g m l q = - m * 9.8 * l * sin q

flipRunPendulum2T :: (Monad m) => ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Pendulum2T m x -> m x
flipRunPendulum2T = flip . flip runPendulum2T

runPendulum2 :: Pendulum2T Identity a -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> a
runPendulum2 pmx (m, l) (q, p) = runIdentity $ runPendulum2T pmx (m, l) (q, p)
flipRunPendulum2 :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Pendulum2T Identity a -> a
flipRunPendulum2 = flip . flip runPendulum2
