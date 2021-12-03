module PendulumN (
  hamiltonian,
  PendulumN, runPendulumN, flipRunPendulumN,
  PendulumNT, runPendulumNT, flipRunPendulumNT,
  module PhysicalSystem
)where
import PhysicalSystem
import Data.Functor.Identity
import Data.Vector.Extra
import Data.List

mass :: (Num a) => Vector a -> Vector a
mass = vector . map sum . tails . toList

hamiltonian :: PendulumN Double
hamiltonian = do
  vml@(vm, vl) <- askData
  vqp@(vq, vp) <- getPhase
  let ve = h <$> mass vm <*> vl <*> vq <*> vp
  return $ sum ve
    where
      h m l q p = p * p / (2 * m * l * l) - m * 9.8 * l * cos q


type PendulumNT m = PhysicalSystemT (Vector Double, Vector Double) (Vector Double) (Vector Double) m
type PendulumN = PendulumNT Identity

runPendulumNT :: (Monad m) => PendulumNT m x -> (Vector Double, Vector Double) -> (Vector Double, Vector Double) -> m x
runPendulumNT = flip runPhysicalSystemT (dqdt, dpdt)
  where
    dqdt (vm, vl) (_, vp) = f <$> mass vm <*> vl <*> vp
    dpdt (vm, vl) (vq, _) = g <$> mass vm <*> vl <*> vq
    f m l p = p / (m * l * l)
    g m l q = - m * 9.8 * l * sin q

flipRunPendulumNT :: (Monad m) => (Vector Double, Vector Double) -> (Vector Double, Vector Double) -> PendulumNT m x -> m x
flipRunPendulumNT = flip . flip runPendulumNT

runPendulumN :: PendulumNT Identity a -> (Vector Double, Vector Double) -> (Vector Double, Vector Double) -> a
runPendulumN pmx (m, l) (q, p) = runIdentity $ runPendulumNT pmx (m, l) (q, p)
flipRunPendulumN :: (Vector Double, Vector Double) -> (Vector Double, Vector Double) -> PendulumNT Identity a -> a
flipRunPendulumN = flip . flip runPendulumN
