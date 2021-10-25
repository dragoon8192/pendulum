module PendlumMonad (
  Pendulum,
)where
import Data.AffineSpace
import Data.VectorSpace
import Control.Arrow
import Data.Functor
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

--type PhaseSpace a = State (a, a)
--getQ :: PhaseSpace a a
--getQ = gets fst
--getP :: PhaseSpace a a
--getP = gets snd
--addQ :: (AffineSpace a) => Diff a -> PhaseSpace a ()
--addQ dq = modify $ first (.+^ dq)
--addP :: (AffineSpace a) => Diff a -> PhaseSpace a ()
--addP dp = modify $ second (.+^ dp)
--evolQ :: (AffineSpace a, VectorSpace (Diff a)) => Scalar (Diff a) -> Diff a -> PhaseSpace a ()
--evolQ dt dqdt = addQ $ dqdt ^* dt
--evolP :: (AffineSpace a, VectorSpace (Diff a)) => Scalar (Diff a) -> Diff a -> PhaseSpace a ()
--evolP dt dpdt = addP $ dpdt ^* dt

class (Monad s, AffineSpace (Q s), AffineSpace (P s), VectorSpace (Diff (Q s)), VectorSpace (Diff (P s)), DTime s ~ Scalar (Diff (P s)), DTime s ~ Scalar (Diff (Q s))) => PhysSystem s where
  type DTime s :: *
  type Q s :: *
  type P s :: *
  getQ :: s (Q s)
  getP :: s (P s)
  evolQ :: DTime s -> s ()
  evolP :: DTime s -> s ()
  getDqDt :: s (Diff (Q s))
  getDpDt :: s (Diff (P s))
  symplecticEvol1 :: DTime s -> s ()
  symplecticEvol1 dt = do
    evolQ dt
    evolP dt

type Pendulum = ReaderT (Double,Double) (State (Double,Double))
runPendulum :: Pendulum a -> Double -> Double -> Double -> Double -> (a, (Double, Double))
runPendulum pendA m l q p = runState (runReaderT pendA (m,l)) (q,p)
execPendulum :: Pendulum a -> Double -> Double -> Double -> Double -> (Double, Double)
execPendulum pendA m l q p = execState (runReaderT pendA (m,l)) (q,p)
getMass :: Pendulum Double
getMass = asks $ fst
getLength :: Pendulum Double
getLength = asks $ snd

instance PhysSystem Pendulum where
  type DTime Pendulum = Double
  type Q Pendulum = Double
  type P Pendulum = Double
  getQ = lift . gets $ fst
  getP = lift . gets $ snd
  getDqDt = dqdt <$> getP <*> getMass <*> getLength
    where
      dqdt p m l = p / (m * l * l)
  getDpDt = dpdt <$> getQ <*> getMass <*> getLength
    where
      dpdt q m l = - m * 9.8 * l * sin q

