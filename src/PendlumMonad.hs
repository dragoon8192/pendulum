module PendlumMonad (
  Pendulum,
)where
import Data.AffineSpace
import Data.VectorSpace
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

class (MonadState (Q s,P s) s, AffineSpace (Q s), AffineSpace (P s), VectorSpace (Diff (Q s)), VectorSpace (Diff (P s)), DTime s ~ Scalar (Diff (P s)), DTime s ~ Scalar (Diff (Q s)))
  => PhysSystem s where
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
  evolQ dt = do
    dqdt <- getDqDt
    modify $ first (.+^ dt *^ dqdt)
    --modify . first . flip (.+^) . (dt *^) =<< getDqDt
  evolP :: DTime s -> s ()
  evolP dt = do
    dpdt <- getDpDt
    modify $ second (.+^ dt *^ dpdt)

  symplecticEvol1 :: DTime s -> s ()
  symplecticEvol1 dt = do
    evolQ dt
    evolP dt

newtype PhysicalSystem d q p x = PhysicalSystem (ReaderT ((d -> (q, p) -> Diff q, d -> (q, p) -> Diff p), d) (State (q, p)) x)
  deriving (Functor, Applicative, Monad, MonadState (q,p))
    --MonadReader ((d -> (q, p) -> Diff q, d -> (q, p) -> Diff p),d))
runPhysicalSystem :: PhysicalSystem d q p x -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> (x, (q, p))
runPhysicalSystem (PhysicalSystem system) (dqdtFunc, dpdtFunc) d (q, p)
  = runState (runReaderT system ((dqdtFunc, dpdtFunc), d)) (q, p)
execPhysicalSystem :: PhysicalSystem d q p x -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> (q, p)
execPhysicalSystem (PhysicalSystem system) (dqdtFunc, dpdtFunc) d (q, p)
  = execState (runReaderT system ((dqdtFunc, dpdtFunc), d)) (q, p)
askData :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p)) => PhysicalSystem d q p d
askData = PhysicalSystem $ asks snd
askDiffFunc :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p))
  => PhysicalSystem d q p (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p)
askDiffFunc = PhysicalSystem $ asks fst

instance (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p), Scalar (Diff q) ~ Scalar (Diff p))
  => PhysSystem (PhysicalSystem d q p) where
  type DTime (PhysicalSystem d q p) = Scalar (Diff q)
  type Q (PhysicalSystem d q p) = q
  type P (PhysicalSystem d q p) = p

  getPhase = get
  getDiffPhase = eval2 <$> askDiffFunc <*> askData <*> getPhase
    where
      eval2 (f,g) d pq = (f d pq, g d pq)

type Pendulum = PhysicalSystem (Double,Double) Double Double
runPendulum system = runPhysicalSystem system (dqdt, dpdt)
  where
    dqdt (m, l) (q, p) = p / (m * l * l)
    dpdt (m, l) (q, p) = - m * 9.8 * l * sin q
