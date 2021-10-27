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

  getPhase :: s (Q s, P s)
  getQ :: s (Q s)
  getQ = fmap fst getPhase
  getP :: s (P s)
  getP = fmap snd getPhase

  getDiffPhase :: s (Diff (Q s), Diff (P s))
  getDqDt :: s (Diff (Q s))
  getDqDt = fmap fst getDiffPhase
  getDpDt :: s (Diff (P s))
  getDpDt = fmap snd getDiffPhase

  evolQ :: DTime s -> s ()
  evolP :: DTime s -> s ()
  symplecticEvol1 :: DTime s -> s ()
  symplecticEvol1 dt = do
    evolQ dt
    evolP dt

newtype PhysicalSystem d q p x = PhysicalSystem (ReaderT (d, (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p)) (State (q, p)) x)
  deriving (Functor, Applicative, Monad, MonadState (q,p))
  --, MonadReader (d, (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p))
--data PhysicalSystem d q p x where
--  PhysicalSystem :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p))
--    => {innerState :: ReaderT (d, (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p)) (State (q, p)) x }
--    -> PhysicalSystem d q p x
runPhysicalSystem :: PhysicalSystem d q p x -> d -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> (q, p) -> (x, (q, p))
runPhysicalSystem (PhysicalSystem system) d (dqdt, dpdt) (q, p) = runState (runReaderT system (d, (dqdt, dpdt))) (q, p)
execPhysicalSystem :: PhysicalSystem d q p x -> d -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> (q, p) -> (q, p)
execPhysicalSystem (PhysicalSystem system) d (dqdt, dpdt) (q, p) = execState (runReaderT system (d, (dqdt, dpdt))) (q, p)
askData :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p)) => PhysicalSystem d q p d
askData = PhysicalSystem $ asks fst
askDiffFunc :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p)) => PhysicalSystem d q p (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p)
askDiffFunc = PhysicalSystem $ asks snd

instance (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p), Scalar (Diff q) ~ Scalar (Diff p)) => PhysSystem (PhysicalSystem d q p) where
  type DTime (PhysicalSystem d q p) = Scalar (Diff q)
  type Q (PhysicalSystem d q p) = q
  type P (PhysicalSystem d q p) = p
  getDiffPhase = do
    (dqdtFunc, dpdtFunc) <- askDiffFunc
    dqdt <- dqdtFunc <$> askData <*> getPhase
    dpdt <- dpdtFunc <$> askData <*> getPhase
    return (dqdt ,dpdt)
  getPhase = PhysicalSystem $ lift get





type Pendulum = PhysicalSystem (Double,Double) Double Double

--instance PhysSystem Pendulum where
--  --type DTime Pendulum = Double
--  type Q Pendulum = Double
--  type P Pendulum = Double
--  getQ = lift . gets $ fst
--  getP = lift . gets $ snd
--  getDqDt = dqdt <$> getP <*> getMass <*> getLength
--    where
--      dqdt p m l = p / (m * l * l)
--  getDpDt = dpdt <$> getQ <*> getMass <*> getLength
--    where
--      dpdt q m l = - m * 9.8 * l * sin q

